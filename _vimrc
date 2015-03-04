execute pathogen#infect()
set softtabstop=4
set tabstop=4
set sw=4
set expandtab
set ic
set ai
set autochdir
set nocp
set nu
set nocompatible
set tags=./tags;/
filetype plugin on
hi clear SpellBad
hi SpellBad cterm=underline,bold ctermfg=magenta

" options for Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
