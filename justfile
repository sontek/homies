help:
  just --list

# Install fun apps
install-fun-apps:
  nix profile install nixpkgs#boxes
  nix profile install nixpkgs#cowsay
  nix profile install nixpkgs#figlet
  nix profile install nixpkgs#fortune
  nix profile install nixpkgs#lolcat
  nix profile install nixpkgs#toilet

# Install apps for every day use
install-system-apps:
  nix profile install nixpkgs#asdf-vm
  nix profile install nixpkgs#bat
  nix profile install nixpkgs#cheat
  nix profile install nixpkgs#exa
  nix profile install nixpkgs#fzf
  nix profile install nixpkgs#gettext
  nix profile install nixpkgs#git
  nix profile install nixpkgs#gnupg
  nix profile install nixpkgs#jq
  nix profile install nixpkgs#neovim
  nix profile install nixpkgs#ripgrep
  nix profile install nixpkgs#stow
  nix profile install nixpkgs#tmux
  nix profile install nixpkgs#xh
  nix profile install nixpkgs#yq
  nix profile install nixpkgs#zsh

# Install apps for doing SRE work
install-sre-apps:
  nix profile install nixpkgs#kubectl
  nix profile install nixpkgs#sops
  nix profile install nixpkgs#terraform
  nix profile install nixpkgs#awscli2
  nix profile install nixpkgs#aws-vault
  nix profile install nixpkgs#argocd


# Install all applications
install: install-system-apps install-sre-apps install-fun-apps
  echo "Done installing all packages"

# Upgrade all installed applications
upgrade:
  nix profile upgrade '.*'

# Installs all the dotfiles
install-dotfiles:
  cd dotfiles && stow --verbose=1 --target=$HOME */

# Removes all the dotfiles
remove-dotfiles:
  cd dotfiles && stow --verbose=1 --delete --target=$HOME */

# Configure ASDF with all desired plugins
setup-asdf:
  asdf list|grep nodejs > /dev/null || asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
  asdf list|grep yarn > /dev/null || asdf plugin add yarn https://github.com/twuni/asdf-yarn.git
  asdf list|grep python > /dev/null || asdf plugin add python https://github.com/danhper/asdf-python.git
  asdf list|grep poetry > /dev/null || asdf plugin add poetry https://github.com/asdf-community/asdf-poetry.git
  asdf list|grep golang > /dev/null || asdf plugin add golang https://github.com/kennyp/asdf-golang.git

# Setup development language environments
setup-dev: setup-asdf

# Remove packages that aren't referenced
nix-gc:
  nix profile wipe-history
  nix-collect-garbage --delete-old
  nix store gc
  nix store optimise

