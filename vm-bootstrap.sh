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

# Symlink whatever project is mounted under /Users/*/code/* to a clean
# ~/code/<name> path (runs as the user; no network needed).
mkdir -p "$HOME/code"
for d in /Users/*/code/*/; do
  [ -d "$d" ] || continue
  ln -sfn "${d%/}" "$HOME/code/$(basename "$d")"
done

# One-time bootstrap: homies prerequisites (just + ripgrep, used by its justfile)
# plus curl/xz-utils, then Determinate Nix. Sentinel-guarded so it runs once.
if [ ! -f /var/lib/.homies-bootstrap-done ]; then
  export DEBIAN_FRONTEND=noninteractive
  sudo -E apt-get update
  sudo -E apt-get -y install curl xz-utils just ripgrep
  if [ ! -e /nix ] && ! command -v nix >/dev/null 2>&1; then
    curl --retry 3 --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
      | sudo sh -s -- install --no-confirm
  fi
  sudo touch /var/lib/.homies-bootstrap-done
fi

echo "[vm-bootstrap] done — just/ripgrep/curl + nix ready; ~/code symlinks in place"
