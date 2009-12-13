" Author: John Anderson (sontek@gmail.com)

" Stop behaving like vi; vim enhancements are better
set nocompatible 

""" Moving Around/Editing
set nostartofline           " Avoid moving cursor to BOL when jumping around
set whichwrap=b,s,h,l,<,>   " <BS> <Space> h l <Left> <Right> can change lines
set virtualedit=block       " Let cursor move past the last char in <C-v> mode
set scrolloff=3             " Keep 3 context lines above and below the cursor
set backspace=2             " Allow backspacing over autoindent, EOL, and BOL
set showmatch               " Briefly jump to a paren once it's balanced
set matchtime=2             " (for only .2 seconds).
set nowrap		    " don't wrap text

""" Searching and Patterns
set ignorecase              " Default to using case insensitive searches,
set smartcase               " unless uppercase letters are used in the regex.
set hlsearch                " Highlight searches by default.
set incsearch               " Incrementally search while typing a /regex
set hlsearch		    " Highlight matched search items

""" Insert completion
" don't select first item, follow typing in autocomplete
set completeopt=longest,menuone 

"""" Folding
set foldmethod=syntax       " By default, use syntax to determine folds
set foldlevelstart=99       " All folds open by default

"""" Display
set number                  " Display line numbers
set numberwidth=1           " using only 1 column (and 1 space) while possible
set background=dark 
set guioptions-=m  	    " remove menu bar
set guioptions-=T      	    " remove toolbar
set guioptions-=r  	    " remove right-hand scroll bar

"""" Messages, Info, Status
set vb t_vb=                " Disable all bells.  I hate ringing/flashing.
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
set ruler                   " Show some info, even without statuslines.
set laststatus=2            " Always show statusline, even if only 1 window.

"""" Tabs/Indent Levels
set autoindent              " Do dumb autoindentation when no filetype is set
set tabstop=8               " Real tab characters are 8 spaces wide,
set shiftwidth=4            " but an indent level is 2 spaces wide.
set softtabstop=4           " <BS> over an autoindent deletes both spaces.
set expandtab               " Use spaces, not tabs, for autoindent/tab key.
set ai			    " auto indent

"""" Tags
set tags=./tags;/home       " Tags can be in ./tags, ../tags, ..., /home/tags.
set showfulltag             " Show more information while completing tags.
set cscopetag               " When using :tag, <C-]>, or "vim -t", try cscope:
set cscopetagorder=0        " try ":cscope find g foo" and then ":tselect foo"
map <silent><C-Left> <C-T>      
map <silent><C-Right> <C-]>

"""" Reading/Writing
set noautowrite             " Never write a file unless I request it.
set noautowriteall          " NEVER.
set noautoread              " Don't automatically re-read changed files.
set modeline                " Allow vim options to be embedded in files;
set modelines=5             " they must be within the first or last 5 lines.
set ffs=unix,dos,mac        " Try recognizing dos, unix, and mac line endings.

"""" Backups/Swap Files
" Make sure that the directory where we want to put swap/backup files exists.
if ! len(glob("~/.backup/"))
  echomsg "Backup directory ~/.backup doesn't exist!"
endif
set writebackup             " Make a backup of the original file when writing
set backup                  " and don't delete it after a succesful write.
set backupskip=             " There are no files that shouldn't be backed up.
set updatetime=2000         " Write swap files after 2 seconds of inactivity.
set backupext=~             " Backup for "file" is "file~"
set backupdir^=~/.backup    " Backups are written to ~/.backup/ if possible.
set directory^=~/.backup//  " Swap files are also written to ~/.backup, too.
" ^ Here be magic! Quoth the help:
" For Unix and Win32, if a directory ends in two path separators "//" or "\\",
" the swap file name will be built from the complete path to the file with all
" path separators substituted to percent '%' signs.  This will ensure file
" name uniqueness in the preserve directory.

"""" Command Line
set history=1000            " Keep a very long command-line history.
set wildmenu                " Menu completion in command mode on <Tab>
set wildmode=full           " <Tab> cycles between all matching choices.
set wcm=<C-Z>               " Ctrl-Z in a mapping acts like <Tab> on cmdline
source $VIMRUNTIME/menu.vim " Load menus (this would be done anyway in gvim)
" <F4> triggers the menus, even in terminal vim.
map <F4> :emenu <C-Z>

"""" Per-Filetype Scripts
" NOTE: These define autocmds, so they should come before any other autocmds.
"       That way, a later autocmd can override the result of one defined here.
filetype on                 " Enable filetype detection,
filetype indent on          " use filetype-specific indenting where available,
filetype plugin on          " also allow for filetype-specific plugins,
syntax on                   " and turn on per-filetype syntax highlighting.



set noic
set textwidth=79
"set showtabline=2
let Tlist_GainFocus_On_ToggleOpen=1
let g:skip_loading_mswin=1

" easily move around tabs
map <silent><A-Right> :tabnext<CR> 
map <silent><A-Left> :tabprevious<CR> 
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
