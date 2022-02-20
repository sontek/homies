alias ls='exa -lah'
alias cat='bat --paging=never'
alias less='bat --paging=always'
alias drun='docker run --rm -it -v "${PWD}":/app -w /app'
alias diff="diff --color -u"
alias vim='nvim'

# Nix Package list
alias npl="nix profile list|awk '{print \$2}'|sort|uniq|sed s/flake:nixpkgs#legacyPackages.x86_64-//g|sed s/darwin\.//g"

# SRE aliases
alias k="kubectl"
alias tf="terraform"
