# man zshmisc for docs

# Load colors so we can access $fg and more.
autoload -U colors && colors


git_prompt() {
    # local branch="$(git symbolic-ref HEAD 2> /dev/null | cut -d'/' -f3)"
    local branch=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    # local branch_truncated="${branch:0:30}"
    #if (( ${#branch} > ${#branch_truncated} )); then
    #    branch="${branch_truncated}..."
    #fi

    [ -n "${branch}" ] && echo " (git:${branch})"
}


kube_prompt()
{
    if command -v kubectl &> /dev/null
    then
        CONTEXT=$(kubectl config current-context)

        if [ -n "$CONTEXT" ]; then
            echo "[k8s:${CONTEXT}]"
        fi
    fi

}

# Allow substitutions in the prompt (like git_prompt)
setopt PROMPT_SUBST

# The %{ %} around this is important.  Otherwise big gaps
# will appear when doing things like reverse search

# %B / %b = start/stop bold
# %~ = path releative to $HOME
NL=$'\n'
PROMPT='%{$fg[yellow]%}%~%{$fg[green]%}$(git_prompt)$(kube_prompt)%(?.%{$fg[blue]%}.%{$fg[red]%})$NL%B❯%b '

