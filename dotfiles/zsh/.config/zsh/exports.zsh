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

# Use newer llvm from homebrew
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

# Enable RTX activations
if command -v mise &> /dev/null
then
    if [[ -o interactive ]]; then
        eval "$(mise activate zsh)"
        eval "$(mise completion zsh)"
    else
        export PATH="$HOME/.local/share/mise/shims:$PATH"
    fi
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

# Setup the KUBECONFIG env var from ~/kubeconfigs, only if it exists and has
# files (it's absent on machines/VMs without kube configs, where an unguarded
# find errors on every shell startup).
if [ -d ~/kubeconfigs ] && [ -n "$(find ~/kubeconfigs -type f 2>/dev/null)" ]; then
  export KUBECONFIG=$(find ~/kubeconfigs -type f|xargs|tr -s '[:blank:]' ':')
fi
. "${XDG_CONFIG_HOME}/zsh/dynamic-exports.zsh"

# In a Colima dev VM (hostname colima-<project>), a fresh `vm-ssh` / `colima ssh`
# login lands in the inherited host path (/Users/.../code/...) or $HOME. Move it
# to the clean ~/code/<project> bind mount so tools that default to the working
# directory (e.g. aoe) report ~/code/<project> instead of the /Users/... mount
# path. Guarded to the VM by hostname, and only from $HOME or a /Users path so it
# never pulls an aoe/worktree shell (already in the project) out of its dir.
if [[ -o interactive && "$HOST" == colima-* ]]; then
  __vm_proj="$HOME/code/${${HOST#colima-}%%.*}"
  if [[ -d "$__vm_proj" && ( "$PWD" == "$HOME" || "$PWD" == /Users/* ) ]]; then
    cd "$__vm_proj"
  fi
  unset __vm_proj
fi

