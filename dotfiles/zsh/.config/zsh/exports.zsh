# Setup the environment to have ASDF controlled
# binaries be that the fron of the path
if command -v asdf &> /dev/null
then
    ASDF_PATH=$(asdf info|grep ASDF_DIR|awk -F'=' '{ print $2}')
    . $ASDF_PATH/asdf.sh
    . $ASDF_PATH/completions/asdf.bash
fi


# Enable completions when using kubectl
if command -v kubectl &> /dev/null
then
    . <(kubectl completion zsh)
    complete -F __start_kubectl k
fi

# Enable completions when using aws
complete -C 'aws_completer' aws

# Load Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

export DYLD_LIBRARY_PATH=$HOME/.nix-profile/lib/

# Add Nix to $PATH
export PATH=$HOME/.nix-profile/bin:$PATH

# Add krew to $PATH
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

eval "$(direnv hook zsh)"

