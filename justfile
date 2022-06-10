help:
  @just --list

# Install apps into nix profile, 'just install-nix "foo bar"
install-nix nix_apps:
  @for app in {{nix_apps}}; do \
      if !(nix profile list | rg -q -F "\.${app}"); then \
          echo "Installing ${app}"; \
          outputs=$(nix eval nixpkgs#${app}.outputs |sed 's/[]["]//g'); \
          for out in $outputs; do \
              echo "Building output ${out} for ${app}"; \
              nix build nixpkgs#${app}.${out}; \
          done; \
          nix profile install nixpkgs#${app}; \
      else \
          echo "Package ${app} already installed, skipping"; \
      fi \
  done

# Install fun apps
install-fun-apps: (install-nix "boxes cowsay figlet fortune lolcat toilet")

# Install apps for every day use
install-system-apps:
  @just install-nix "asdf-vm"
  @just install-nix "bash"
  @just install-nix "bat"
  @just install-nix "cheat"
  @just install-nix "colordiff"
  @just install-nix "coreutils"
  @just install-nix "direnv"
  @just install-nix "exa"
  @just install-nix "ffmpeg"
  @just install-nix "fzf"
  @just install-nix "gettext"
  @just install-nix "git"
  @just install-nix "gnupg"
  @just install-nix "jless"
  @just install-nix "jq"
  @just install-nix "loc"
  @just install-nix "lz4"
  @just install-nix "minikube"
  @just install-nix "ncurses"
  @just install-nix "neovim"
  @just install-nix "ripgrep"
  @just install-nix "sd"
  @just install-nix "stow"
  @just install-nix "tmux"
  @just install-nix "watch"
  @just install-nix "xh"
  @just install-nix "yq"
  @just install-nix "zsh"

# Install apps for doing SRE work
install-sre-apps:
  @just install-nix "argocd awscli2 aws-vault dos2unix krew kubie"
  @just install-nix "kubecolor redis sops stern teleport"

# Install all applications
install: install-system-apps install-sre-apps install-fun-apps
  @echo "Done installing all packages"

# Upgrade all installed applications
nix-upgrade:
  nix profile upgrade '.*'

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

asdf_plugins := "nodejs python golang helm yarn poetry kubectl terraform"
# Configure ASDF with all desired plugins
setup-asdf:
  @for plugin in {{asdf_plugins}}; do \
      asdf plugin list|grep ${plugin} > /dev/null; \
      if [ $? -ne 0 ]; then \
          echo 'Adding asdf' ${plugin}; \
          asdf plugin add ${plugin}; \
      else \
          echo "Asdf plugin ${plugin} already installed"; \
      fi \
  done

install-asdf-versions:
    asdf install kubectl 1.23.5
    asdf install kubectl 1.21.10
    asdf install python 3.8.13
    asdf install python 3.9.12
    asdf install nodejs 12.22.12
    asdf install nodejs 16.14.2
    asdf install golang 1.17.8
    asdf install golang 1.18
    asdf install terraform 1.1.8
    asdf install yarn 1.22.18
    asdf install poetry 1.1.13

# Setup development language environments
setup-dev: setup-asdf install-asdf-versions

setup-krew:
    krew install krew
    kubectl krew install neat
    kubectl krew install oidc-login
    kubectl krew install popeye
    kubectl krew install resource-capacity
    kubectl krew install ctx
