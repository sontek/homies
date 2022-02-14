default:
  just --list

# Install fun apps
install-fun-apps:
  nix profile install nixpkgs#boxes
  nix profile install nixpkgs#cowsay
  nix profile install nixpkgs#figlet
  nix profile install nixpkgs#lolcat
  nix profile install nixpkgs#toilet

# Install apps for every day use
install-system-apps:
  nix profile install nixpkgs#asdf
  nix profile install nixpkgs#bat
  nix profile install nixpkgs#cheat
  nix profile install nixpkgs#exa
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
  nix profile install nixpkgs#argocd


# Install all applications
install: install-system-apps install-sre-apps install-fun-apps
  echo "Done installing all packages"

nix-gc:
  nix profile wipe-history
  nix-collect-garbage --delete-old
  nix store gc
  nix store optimise

