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

# A remote program (tmux, vim) that enables mouse reporting and then dies with
# its SSH connection never sends the codes to turn it off, so the terminal
# keeps typing escape codes like "35;82;44M" whenever the mouse moves. Turn
# mouse reporting and focus events off before each prompt to recover.
_reset_mouse_reporting() {
  printf '\e[?1000l\e[?1002l\e[?1003l\e[?1004l\e[?1006l\e[?1015l'
}
autoload -Uz add-zsh-hook
add-zsh-hook precmd _reset_mouse_reporting
