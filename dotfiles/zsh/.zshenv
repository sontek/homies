export XDG_CONFIG_HOME="${HOME}/.config"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
source ${ZDOTDIR}/.zshrc

# Use this sparingly, nix is preferred, but have it available
# just in case.
eval "$(/opt/homebrew/bin/brew shellenv)"
