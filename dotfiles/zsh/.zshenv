export XDG_CONFIG_HOME="${HOME}/.config"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
source ${ZDOTDIR}/.zshrc

# Use this sparingly, nix is preferred, but have it available
# just in case.
if command -v brew &> /dev/null
then
    eval "$(brew shellenv)"
fi

ulimit -n 4096

# Force OSC 8 hyperlink output for CLIs that probe terminal support (Claude Code's
# clickable "PR #NNNN" footer link, etc.). Needed inside SSH'd VMs (e.g. colima),
# where TERM degrades to xterm-256color and kitty's terminfo is absent, so tools
# can't detect that the real terminal at the end of the pipe (kitty) supports
# hyperlinks. SSH passes the escapes through untouched. Interactive-only to avoid
# leaking escapes into redirected output of non-interactive scripts.
if [[ -t 1 ]]; then
    export FORCE_HYPERLINK=1
fi
