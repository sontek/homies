set nocompatible
set background=dark
set nowrap
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set completeopt=longest,menuone " don't select first item, follow typing
set noic
set number
set ai
set softtabstop=4
set tabstop=8
set shiftwidth=4
set textwidth=79
"set showtabline=2
let Tlist_GainFocus_On_ToggleOpen=1
let g:skip_loading_mswin=1

syntax on

filetype on
filetype plugin on

map <silent><A-Right> :tabnext<CR> 
map <silent><A-Left> :tabprevious<CR> 
map <silent><C-Left> <C-T>      
map <silent><C-Right> <C-]>
map <C-h> :py EvaluateCurrentRange()<CR>
map T :TaskList<CR><C-w><Left>
map <F4> :TlistToggle<CR>
hi clear

autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd BufRead *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"      
autocmd BufRead *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m       

inoremap <Nul> <C-x><C-o>
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
	\ "\<lt>C-n>" :
	\ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
	\ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
	\ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
imap <C-@> <C-Space>

python << EOF
import os
import sys
import vim
# lets us use 'gf' to go to files imported    
for p in sys.path:
    if os.path.isdir(p):
        vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))

# lets us execute the highlighted portion of the script
def EvaluateCurrentRange():       
  eval(compile('\n'.join(vim.current.range),'','exec'),globals())       
EOF
