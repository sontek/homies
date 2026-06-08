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

# Bind-mount whatever project is mounted under /Users/*/code/* onto a clean
# ~/code/<name> path. A bind mount (not a symlink) is deliberate: getcwd()
# resolves a symlink back to /Users/..., so tools that key off the working
# directory (aoe defaults a new session to the current directory) would report
# the ugly mount path. A bind mount is a real mount point, so the cwd stays
# ~/code/<name>. Bind mounts don't persist across a VM stop, so this re-runs on
# every `vm-up`. Old symlinks from earlier bootstraps are converted to mounts.
log "bind-mounting mounted projects into ~/code ..."
mkdir -p "$HOME/code"
for d in /Users/*/code/*/; do
  [ -d "$d" ] || continue
  name="$(basename "$d")"
  target="$HOME/code/$name"
  [ -L "$target" ] && rm -f "$target"
  mkdir -p "$target"
  if mountpoint -q "$target"; then
    echo "    ~/code/$name already mounted"
  else
    sudo mount --bind "${d%/}" "$target"
    echo "    ~/code/$name -> ${d%/} (bind)"
  fi
done

# One-time bootstrap: homies prerequisites (just + ripgrep, used by its justfile)
# plus curl/xz-utils, then Determinate Nix. Sentinel-guarded so it runs once.
if [ -f /var/lib/.homies-bootstrap-done ]; then
  log "system bootstrap already done (sentinel present) — skipping apt + nix"
else
  export DEBIAN_FRONTEND=noninteractive
  # build-essential gives us a C toolchain (cc/make) so Neovim plugins with
  # native components compile in-VM: telescope-fzf-native's libfzf.so and
  # nvim-treesitter parsers. Without it :PackerSync fails to load the fzf
  # extension ("cannot open shared object file: libfzf.so").
  log "apt: updating index + installing curl xz-utils just ripgrep zsh build-essential ..."
  sudo -E apt-get update
  sudo -E apt-get -y install curl xz-utils just ripgrep zsh build-essential
  if [ ! -e /nix ] && ! command -v nix >/dev/null 2>&1; then
    log "nix: installing Determinate Nix (this is the slow step) ..."
    curl --retry 3 --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
      | sudo sh -s -- install --no-confirm
  else
    log "nix: already installed — skipping"
  fi
  # Make zsh the login shell so `colima ssh` / `just vm-ssh` lands in zsh.
  ZSH_BIN="$(command -v zsh || true)"
  if [ -n "$ZSH_BIN" ]; then
    grep -qxF "$ZSH_BIN" /etc/shells || echo "$ZSH_BIN" | sudo tee -a /etc/shells >/dev/null
    sudo chsh -s "$ZSH_BIN" "$(whoami)"
    log "login shell set to $ZSH_BIN"
  fi
  sudo touch /var/lib/.homies-bootstrap-done
  log "system bootstrap complete (sentinel written)"
fi

log "done: just/ripgrep/curl + nix ready; ~/code bind mounts in place"
