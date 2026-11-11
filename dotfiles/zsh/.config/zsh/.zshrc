# Keep the terminal's real TERM (kitty, tmux, screen) when its terminfo entry
# resolves; only fall back to a universally-present 256color entry when it
# doesn't — so a bare/unknown TERM can't break `clear` and friends with
# "unknown terminal type", without clobbering capable terminals.
infocmp "$TERM" >/dev/null 2>&1 || export TERM="xterm-256color"
export PS1="> "

# Ensure core system paths are always present (GUI-launched shells
# like Conductor may not inherit them)
[[ ":$PATH:" != *":/usr/bin:"* ]] && export PATH="$PATH:/usr/bin:/bin:/usr/sbin:/sbin"

# Enable bash compatible completions for
# apps that don't have zsh specific ones
autoload -U +X bashcompinit && bashcompinit

# Enable standard zsh completions
autoload -U +X compinit && compinit

[ -f "${XDG_CONFIG_HOME}/zsh/aliases.zsh" ] && . "${XDG_CONFIG_HOME}/zsh/aliases.zsh"
[ -f "${XDG_CONFIG_HOME}/zsh/exports.zsh" ] && . "${XDG_CONFIG_HOME}/zsh/exports.zsh"

# Using starship now
# [ -f "${XDG_CONFIG_HOME}/zsh/prompt.zsh" ] && . "${XDG_CONFIG_HOME}/zsh/prompt.zsh"

export PATH="$HOME/.local/bin:$PATH"
