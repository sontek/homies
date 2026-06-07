#!/usr/bin/env bash
# Provision a Colima dev VM AFTER it has booted, when DNS/networking is up.
# Piped over ssh by the `just vm-up` / `just vm-bootstrap` recipes:
#   colima ssh -p <vm> -- bash -s < vm-bootstrap.sh
#
# Why post-start instead of a Colima `provision:` boot script: cloud-init
# provision scripts run before the VM's DNS service is ready, so apt's index
# comes back partial ("Unable to locate package just/ripgrep") and slow apt/Nix
# can blow past lima's boot-script timeout and fail the whole `colima start`.
# Running it here makes the VM start fast + reliable, streams output to your
# terminal, and a failure never blocks the VM — just re-run `just vm-bootstrap`.
#
# Idempotent: safe to run on every `vm-up` and to re-run by hand.
set -euo pipefail

log() { printf '\n==> [vm-bootstrap] %s\n' "$*"; }

log "starting on $(hostname) (user $(whoami))"

# Symlink whatever project is mounted under /Users/*/code/* to a clean
# ~/code/<name> path (runs as the user; no network needed).
log "linking mounted projects into ~/code ..."
mkdir -p "$HOME/code"
for d in /Users/*/code/*/; do
  [ -d "$d" ] || continue
  ln -sfn "${d%/}" "$HOME/code/$(basename "$d")"
  echo "    ~/code/$(basename "$d") -> ${d%/}"
done

# One-time bootstrap: homies prerequisites (just + ripgrep, used by its justfile)
# plus curl/xz-utils, then Determinate Nix. Sentinel-guarded so it runs once.
if [ -f /var/lib/.homies-bootstrap-done ]; then
  log "system bootstrap already done (sentinel present) — skipping apt + nix"
else
  export DEBIAN_FRONTEND=noninteractive
  log "apt: updating index + installing curl xz-utils just ripgrep ..."
  sudo -E apt-get update
  sudo -E apt-get -y install curl xz-utils just ripgrep
  if [ ! -e /nix ] && ! command -v nix >/dev/null 2>&1; then
    log "nix: installing Determinate Nix (this is the slow step) ..."
    curl --retry 3 --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
      | sudo sh -s -- install --no-confirm
  else
    log "nix: already installed — skipping"
  fi
  sudo touch /var/lib/.homies-bootstrap-done
  log "system bootstrap complete (sentinel written)"
fi

log "done — just/ripgrep/curl + nix ready; ~/code symlinks in place"
