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

# Expose the read-only ~/screenshots host mount under the guest home too. It
# lands at its literal host path (/Users/<you>/screenshots) — which is what a
# dragged screenshot's path already points to — but the guest home differs
# (/home/<you>.guest), so a bare `~/screenshots` wouldn't resolve. Symlink it so
# the short path works inside the VM. A symlink (not a bind mount) is fine here:
# we only ever read files by path, never cd into it as a session cwd.
for s in /Users/*/screenshots; do
  [ -d "$s" ] || continue
  ln -sfn "$s" "$HOME/screenshots"
  echo "    ~/screenshots -> $s (symlink)"
  break
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
  # Headless-Chromium system libraries for Playwright. Projects that render with
  # a headless browser (docs sites using mkdocs-to-pdf / mermaid, screenshot
  # tooling, etc.) ultimately run `playwright install chromium`, and the
  # downloaded Chromium needs these .so's to launch. Installing them once here
  # means that later install works without --with-deps (which shells out to sudo
  # at runtime). The Chromium binary itself is downloaded per-project into the
  # shared ~/.cache/ms-playwright, so it's cached machine-wide after first use.
  # This is Playwright's Chromium dep set for Ubuntu 24.04 (noble); regenerate
  # with `playwright install-deps --dry-run chromium` if it drifts.
  log "apt: installing Playwright headless-Chromium system libraries ..."
  sudo -E apt-get -y install --no-install-recommends \
    libnss3 libnspr4 libatk1.0-0t64 libatk-bridge2.0-0t64 libcups2t64 \
    libxkbcommon0 libatspi2.0-0t64 libxcomposite1 libxdamage1 libxfixes3 \
    libxrandr2 libgbm1 libasound2t64 libpango-1.0-0 libcairo2 libdbus-1-3 \
    libdrm2 libxcb1 libx11-6 libxext6 libglib2.0-0t64 \
    fonts-liberation fonts-noto-color-emoji xvfb
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

# Claude Code + our skills. Install the CLI (native installer, self-updating)
# when missing, then register ~/code/sontek/sontek-skills as a plugin
# marketplace and enable it, so our skills and subagents are available the
# moment you run `claude` in the VM. Both steps are idempotent and run on every
# bootstrap so a recreated VM self-heals; plugin registration is skipped when
# the skills tree isn't mounted. python3 (system) merges the JSON so existing
# settings are preserved.
if ! command -v claude >/dev/null 2>&1; then
  log "claude: installing Claude Code (native installer) ..."
  curl -fsSL https://claude.ai/install.sh | bash
else
  log "claude: already installed, skipping"
fi

SKILLS_DIR="$HOME/code/sontek/sontek-skills"
if [ -d "$SKILLS_DIR" ]; then
  log "claude: registering sontek-skills plugin marketplace ..."
  PYBIN=/usr/bin/python3; [ -x "$PYBIN" ] || PYBIN=python3
  "$PYBIN" - "$SKILLS_DIR" <<'PY'
import json, os, sys
skills = sys.argv[1]
cfg_dir = os.path.expanduser("~/.claude")
cfg = os.path.join(cfg_dir, "settings.json")
os.makedirs(cfg_dir, exist_ok=True)
data = {}
if os.path.exists(cfg):
    try:
        with open(cfg) as f:
            data = json.load(f)
    except (ValueError, OSError):
        data = {}
data.setdefault("extraKnownMarketplaces", {})["sontek-skills-local"] = {
    "source": {"source": "directory", "path": skills}
}
data.setdefault("enabledPlugins", {})["sontek-skills@sontek-skills-local"] = True
with open(cfg, "w") as f:
    json.dump(data, f, indent=2)
    f.write("\n")
print("    sontek-skills@sontek-skills-local enabled -> " + skills)
PY
else
  log "claude: $SKILLS_DIR not mounted, skipping plugin registration"
fi

log "done: just/ripgrep/curl + nix + claude ready; ~/code bind mounts in place"
