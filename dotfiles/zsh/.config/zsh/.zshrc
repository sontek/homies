# Upgrade a bare/downgraded TERM (e.g. the plain "xterm" colima ssh hands the VM)
# to 256color, but leave richer values (xterm-kitty, tmux-256color, screen-*) alone
# so we don't clobber the terminal's real capabilities or tmux's default-terminal.
[[ -z "$TERM" || "$TERM" == "xterm" ]] && export TERM="xterm-256color"
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
