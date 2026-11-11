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

# Mac screenshots on a Linux box over SSH (see README).
# macOS: tell the Linux boxes we ssh into which user and screenshots folder to
# mount. SendEnv in ~/.ssh/config passes these on; sshd accepts LC_* by
# default. Set MAC_SCREENSHOTS to use a folder other than ~/screenshots.
if [[ $OSTYPE == darwin* ]]; then
  export LC_MAC_USER="$USER"
  export LC_MAC_SCREENSHOTS="${MAC_SCREENSHOTS:-$HOME/screenshots}"
fi

# Linux, over SSH from a Mac: mount that folder at the same path through the
# reverse tunnel (RemoteForward 2222 and the mac-tunnel host in ~/.ssh/config),
# so dragged screenshot paths resolve here unchanged. flock keeps concurrent
# shells from racing; sshfs daemonizes once mounted, which releases the lock.
# Errors go to the log, not the terminal.
if [[ $OSTYPE == linux* && -o interactive && -n $SSH_CONNECTION ]] \
   && [[ -n $LC_MAC_USER && -d $LC_MAC_SCREENSHOTS ]] \
   && ! mountpoint -q "$LC_MAC_SCREENSHOTS" \
   && ss -Hltn 'sport = :2222' | grep -q .; then
  flock -n "${XDG_RUNTIME_DIR:-/tmp}/mac-screenshots.lock" \
    sshfs -o reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,BatchMode=yes \
    "${LC_MAC_USER}@mac-tunnel:${LC_MAC_SCREENSHOTS}" "$LC_MAC_SCREENSHOTS" \
    >>"${XDG_CACHE_HOME:-$HOME/.cache}/mac-screenshots-mount.log" 2>&1 &!
fi
