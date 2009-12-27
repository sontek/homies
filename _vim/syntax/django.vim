" Vim syntax file
" Language:	Django template
" Maintainer:	Dave Hodder <dmh@dmh.org.uk>
" Last Change:	2009 Jan 12

" matchit.vim (extended "%" matching) contributed by Michael Brown.
" Borrowed from html.vim Thanks to Johannes Zellner and Benji Fisher.
if exists("loaded_matchit")
    let b:match_ignorecase = 1
    let b:match_skip = 's:Comment'
    let b:match_words = '<:>,' .
    \ '<\@<=[ou]l\>[^>]*\%(>\|$\):<\@<=li\>:<\@<=/[ou]l>,' .
    \ '<\@<=dl\>[^>]*\%(>\|$\):<\@<=d[td]\>:<\@<=/dl>,' .
    \ '<\@<=\([^/][^ \t>]*\)[^>]*\%(>\|$\):<\@<=/\1>,'  .
    \ '{% *if .*%}:{% *else *%}:{% *endif *%},' .
    \ '{% *ifequal .*%}:{% *else *%}:{% *endifequal *%},' .
    \ '{% *ifnotequal .*%}:{% *else *%}:{% *endifnotequal *%},' .
    \ '{% *ifchanged .*%}:{% *else *%}:{% *endifchanged *%},' .
    \ '{% *for .*%}:{% *endfor *%},' .
    \ '{% *with .*%}:{% *endwith *%},' .
    \ '{% *comment .*%}:{% *endcomment *%},' .
    \ '{% *block .*%}:{% *endblock *%},' .
    \ '{% *filter .*%}:{% *endfilter *%},' .
    \ '{% *spaceless .*%}:{% *endspaceless *%}'
endif
