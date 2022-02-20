export PS1="> "

# Enable bash compatible completions for
# apps that don't have zsh specific ones
autoload -U +X bashcompinit && bashcompinit

# Enable standard zsh completions
autoload -U +X compinit && compinit

[ -f "${XDG_CONFIG_HOME}/zsh/aliases.zsh" ] && . "${XDG_CONFIG_HOME}/zsh/aliases.zsh"
[ -f "${XDG_CONFIG_HOME}/zsh/exports.zsh" ] && . "${XDG_CONFIG_HOME}/zsh/exports.zsh"
[ -f "${XDG_CONFIG_HOME}/zsh/prompt.zsh" ] && . "${XDG_CONFIG_HOME}/zsh/prompt.zsh"

