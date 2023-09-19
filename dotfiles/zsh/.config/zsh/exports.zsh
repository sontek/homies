# Setup the environment to have ASDF controlled
# binaries be that the fron of the path
# Use rtx now
#if command -v asdf &> /dev/null
#then
#    ASDF_PATH=$(asdf info|grep ASDF_DIR|awk -F'=' '{ print $2}')
#    . $ASDF_PATH/asdf.sh
#    . $ASDF_PATH/completions/asdf.bash
#fi


# Enable completions when using kubectl
if command -v kubectl &> /dev/null
then
    . <(kubectl completion zsh)
    complete -F __start_kubectl k
fi

# Enable completions when using aws
complete -C 'aws_completer' aws

export NIXPKGS_ALLOW_UNFREE=1
# Load Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

# Add Nix to $PATH
export PATH=$HOME/.nix-profile/bin:$PATH

# Add krew to $PATH
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# Add rust to the $PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Add hombrew
export PATH="$PATH:/opt/homebrew/bin"

# Enable RTX activations
if command -v rtx &> /dev/null
then
    eval "$(rtx activate zsh)"
    eval "$(rtx completion zsh)"
fi

# Setup starship for a nice prompt
if command -v starship &> /dev/null
then
    eval "$(starship init zsh)"
fi


eval "$(direnv hook zsh)"

# Use ipdb by default when debugging python
export PYTHONBREAKPOINT=ipdb.set_trace

# Load dynamic files that we generate from libs.sh that isn't committed to 
# git.
if ! [[ -f "${XDG_CONFIG_HOME}/zsh/dynamic-exports.zsh" ]]
then
    echo "Regenerating dynamic exports"
    ${XDG_CONFIG_HOME}/zsh/libs.sh
fi

# Load atuin for history
if command -v atuin &> /dev/null
then
    eval "$(atuin init zsh --disable-up-arrow)"
fi

# Setup the KUBECONFIG env var to use ~/kubeconfigs
export KUBECONFIG=$(find ~/kubeconfigs -type f|xargs|tr -s '[:blank:]' ':')
. "${XDG_CONFIG_HOME}/zsh/dynamic-exports.zsh"

