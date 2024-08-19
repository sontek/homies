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
install-fun-apps: (install-nix "boxes cowsay figlet fortune lolcat toilet")

# Install apps for every day use
install-system-apps:
  @just install-nix "asciinema"
  @just install-nix "atuin"
  @just install-nix "azure-cli"
  @just install-nix "bash"
  @just install-nix "bat"
  @just install-nix "bottom"
  @just install-nix "cheat"
  @just install-nix "cmake"
  @just install-nix "colordiff"
  @just install-nix "coreutils"
  @just install-nix "cue"
  @just install-nix "delta"
  @just install-nix "direnv"
  @just install-nix "docker-ls"
  @just install-nix "duf"
  @just install-nix "earthly"
  @just install-nix "eza"
  @just install-nix "ffmpeg"
  @just install-nix "fontconfig"
  @just install-nix "fzf"
  @just install-nix "gettext"
  @just install-nix "gh"
  @just install-nix "git"
  @just install-nix "git-crypt"
  @just install-nix "gnumake"
  @just install-nix "gnupg"
  @just install-nix "graphviz"
  @just install-nix "harfbuzz"
  @just install-nix "heroku"
  @just install-nix "icu72"
  @just install-nix "jless"
  @just install-nix "k6"
  @just install-nix "k9s"
  @just install-nix "libgit2"
  @just install-nix "libffi"
  @just install-nix "loc"
  @just install-nix "lz4"
  @just install-nix "minikube"
  @just install-nix "ncurses"
  @just install-nix "ngrok" "--impure"
  @just install-nix "openssl"
  @just install-nix "pandoc"
  @just install-nix "pango"
  @just install-nix "pkg-config"
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
