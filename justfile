help:
  @just --list

# Install apps into nix profile, 'just install-nix "foo bar"
install-nix nix_apps:
  @for app in {{nix_apps}}; do \
      echo "Installing ${app}"; \
      nix profile install nixpkgs#${app}; \
  done

# Install fun apps
install-fun-apps: (install-nix "boxes cowsay figlet fortune lolcat toilet")

# Install apps for every day use
install-system-apps:
  @just install-nix "asdf-vm"
  @just install-nix "bat"
  @just install-nix "cheat"
  @just install-nix "exa"
  @just install-nix "fzf"
  @just install-nix "gettext"
  @just install-nix "git"
  @just install-nix "gnupg"
  @just install-nix "jless"
  @just install-nix "jq"
  @just install-nix "neovim"
  @just install-nix "ripgrep"
  @just install-nix "sd"
  @just install-nix "stow"
  @just install-nix "tmux"
  @just install-nix "xh"
  @just install-nix "yq"
  @just install-nix "zsh"

# Install apps for doing SRE work
install-sre-apps: (install-nix "argocd awscli2 aws-vault kubectl sops terraform")

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

asdf_plugins := "nodejs python golang helm yarn poetry"
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

# Setup development language environments
setup-dev: setup-asdf

