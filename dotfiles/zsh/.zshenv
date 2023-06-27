export XDG_CONFIG_HOME="${HOME}/.config"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
source ${ZDOTDIR}/.zshrc

# Use this sparingly, nix is preferred, but have it available
# just in case.
if command -v brew &> /dev/null
then
    eval "$(brew shellenv)"
fi

