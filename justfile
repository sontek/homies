set dotenv-load

help:
  @just --list

# Install apps into nix profile, 'just install-nix "foo bar"
install-nix nix_apps args="":
  @for app in {{nix_apps}}; do \
      if !(nix profile list | rg -q -F "\.${app}"); then \
          echo "Installing ${app}"; \
          outputs=$(nix eval nixpkgs#${app}.outputs |sed 's/[]["]//g'); \
          for out in $outputs; do \
              echo "Building output ${out} for ${app}"; \
              nix build {{ args }} nixpkgs#${app}.${out}; \
          done; \
          nix profile install {{ args }} nixpkgs#${app}; \
      else \
          echo "Package ${app} already installed, skipping"; \
      fi \
  done

# Install fun apps
install-fun-apps: (install-nix "boxes cowsay figlet fortune lolcat toilet neofetch")

# Install apps for every day use
install-system-apps:
  @just install-nix "act"
  @just install-nix "aider-chat"
  @just install-nix "asciinema"
  @just install-nix "asciinema-agg"
  @just install-nix "ast-grep"
  @just install-nix "atuin"
  @just install-nix "azure-cli"
  @just install-nix "bash"
  @just install-nix "bat"
  @just install-nix "bottom"
  @just install-nix "cheat"
  @just install-nix "cmake"
  @just install-nix "cairo"
  @just install-nix "colima"
  @just install-nix "colordiff"
  @just install-nix "coreutils"
  @just install-nix "crane"
  @just install-nix "cue"
  @just install-nix "delta"
  @just install-nix "direnv"
  @just install-nix "docker-client"
  @just install-nix "docker-compose"
  @just install-nix "docker-credential-helpers"
  @just install-nix "docker-ls"
  @just install-nix "duf"
  @just install-nix "dwdiff"
  @just install-nix "earthly"
  @just install-nix "easyrsa"
  @just install-nix "eza"
  @just install-nix "ffmpeg"
  @just install-nix "fontconfig"
  @just install-nix "freetype"
  @just install-nix "fzf"
  @just install-nix "google-cloud-sdk"
  @just install-nix "gettext"
  @just install-nix "gh"
  @just install-nix "git"
  @just install-nix "git-crypt"
  @just install-nix "gnumake"
  @just install-nix "gnupg"
  @just install-nix "graphviz"
  @just install-nix "harfbuzz"
  @just install-nix "heroku"
  @just install-nix "htop"
  @just install-nix "icu"
  @just install-nix "imagemagick"
  @just install-nix "jless"
  @just install-nix "k6"
  @just install-nix "k9s"
  @just install-nix "libgit2"
  @just install-nix "libffi"
  @just install-nix "libpng"
  @just install-nix "tokei"
  @just install-nix "lz4"
  @just install-nix "minikube"
  @just install-nix "ncurses"
  @just install-nix "ngrok" "--impure"
  @just install-nix "openssl"
  @just install-nix "openvpn"
  @just install-nix "pandoc"
  @just install-nix "pango"
  @just install-nix "pdfgrep"
  @just install-nix "pixman"
  @just install-nix "pkg-config"
  @just install-nix "poppler-utils"
  @just install-nix "postgresql.dev"
  @just install-nix "protobuf"
  @just install-nix "pwgen"
  @just install-nix "neovim"
  @just install-nix "readline"
  @just install-nix "ripgrep"
  @just install-nix "rtx"
  @just install-nix "sd"
  @just install-nix "shellcheck"
  @just install-nix "starship"
  @just install-nix "stow"
  @just install-nix "terraform-docs"
  @just install-nix "tree"
  @just install-nix "tmux"
  @just install-nix "unzip"
  @just install-nix "util-linux"
  @just install-nix "visidata"
  @just install-nix "watch"
  @just install-nix "watchman"
  @just install-nix "wget"
  @just install-nix "xh"
  @just install-nix "xz"
  @just install-nix "zellij"
  @just install-nix "zsh"

# Install apps for doing SRE work
install-sre-apps:
  @just install-nix "argocd awscli2 aws-nuke aws-vault dos2unix krew kubie"
  @just install-nix "kind kubecolor redis sops stern teleport terraformer"
  @just install-nix "qemu helm-docs kafkactl cilium-cli"
  @just install-nix "ssm-session-manager-plugin"
  @just install-nix "mage golangci-lint e1s granted"

# Install all applications
install: install-system-apps install-sre-apps install-fun-apps
  @echo "Done installing all packages"

# Upgrade all installed applications
nix-upgrade:
  nix profile upgrade '.*' --impure

# Repair the nix store
nix-repair:
  nix-store --repair --verify --check-contents

# Remove packages that aren't referenced
nix-gc:
  nix profile wipe-history
  nix-collect-garbage --delete-old
  nix store gc
  nix store optimise

# Installs all the dotfiles
install-dotfiles:
  cd dotfiles && stow --verbose=1 --target=$HOME */

# Removes all the dotfiles
remove-dotfiles:
  cd dotfiles && stow --verbose=1 --delete --target=$HOME */

# --- Dev VMs (Colima) ----------------------------------------------------
# One Linux VM per project, NAMED AFTER its ~/code subdir, so each VM mounts
# ONLY that project and can't see your other projects or $HOME:
#   `just vm-up drape`     -> profile "drape",    mounts ~/code/drape
#   `just vm-up stacklet`  -> profile "stacklet", mounts ~/code/stacklet
# vz/virtiofs + the no-$HOME default also come from the stowed template
# (~/.config/colima/_templates/default.yaml); the --mount here narrows it to the
# one project (a flag mount replaces the template's broader default).
#
# RAM note: an 18GiB host can't comfortably run two 8GiB VMs at once, so the
# defaults are modest (6 CPU / 6 GiB) to let two coexist. Bump per-invocation
# for a single heavy VM: `just vm-up drape 8 12`. Sizing changes apply after a
# down + up. Disk is thin-provisioned, so 100 is a ceiling, not upfront cost.
#
# Usage: vm-up <name> [cpu] [mem] [disk] | vm-down/vm-ssh/vm-status <name> | vm-list
#
# Create or start a project VM (Colima profile + ~/code/<name> mount), then
# provision it. Bootstrap runs AFTER start (over ssh) because Colima's boot-time
# provision scripts run before DNS is up; post-start it's reliable + visible.
vm-up name cpu="6" memory="6" disk="100":
  colima start {{name}} --runtime docker --vm-type vz --mount-type virtiofs --ssh-agent \
    --cpu {{cpu}} --memory {{memory}} --disk {{disk}} \
    --mount "$HOME/code/{{name}}:w"
  @just vm-bootstrap {{name}}

# Provision a running VM (idempotent): ~/code symlinks, apt prereqs (just/rg/curl),
# Nix. Streams output here. Re-runnable any time; also used by `vm-up`.
vm-bootstrap name:
  colima ssh -p {{name}} -- bash -s < {{justfile_directory()}}/vm-bootstrap.sh

# Stop a VM (frees RAM/CPU; disk + everything installed inside persists)
vm-down name:
  colima stop {{name}}

# Shell into a VM
vm-ssh name:
  colima ssh -p {{name}}

# Show a VM's status and address
vm-status name:
  colima status {{name}}

# List all VMs and their state
vm-list:
  colima list

asdf_plugins := "nodejs python golang helm yarn poetry kubectl kustomize terraform terragrunt postgres pnpm sentinel skaffold tilt rust kops yq jq"
# Configure ASDF with all desired plugins
setup-asdf:
  @for plugin in {{asdf_plugins}}; do \
      asdf plugin list|grep ${plugin} > /dev/null; \
      if [ $? -ne 0 ]; then \
          echo 'Adding asdf' ${plugin}; \
          asdf plugin add ${plugin}; \
      else \
          echo "Asdf plugin ${plugin} already installed. Removing it"; \
          asdf plugin remove ${plugin}; \
          asdf plugin add ${plugin}; \
      fi \
  done

# Setup development language environments
setup-dev: setup-asdf

setup-krew:
    krew install krew
    kubectl krew install neat
    kubectl krew install oidc-login
    kubectl krew install resource-capacity
    kubectl krew install ctx
