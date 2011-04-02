let mapleader = ","
""" Moving Around/Editing
set nostartofline           " Avoid moving cursor to BOL when jumping around
set virtualedit=block       " Let cursor move past the last char in <C-v> mode
set scrolloff=3             " Keep 3 context lines above and below the cursor
set backspace=2             " Allow backspacing over autoindent, EOL, and BOL
set showmatch               " Briefly jump to a paren once it's balanced
set matchtime=2             " (for only .2 seconds).
set nowrap                  " don't wrap text
set linebreak               " don't wrap textin the middle of a word

""" Searching and Patterns
set ignorecase              " Default to using case insensitive searches,
set smartcase               " unless uppercase letters are used in the regex.
set hlsearch                " Highlight searches by default.
set incsearch               " Incrementally search while typing a /regex

""" Insert completion
" don't select first item, follow typing in autocomplete
set completeopt=longest,menuone,preview
set pumheight=6             " Keep a small completion window

" close preview window automatically
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

"""" Folding
set foldmethod=indent       " By default, use syntax to determine folds
set foldlevelstart=99       " All folds open by default


set number                  " Display line numbers
set numberwidth=1           " using only 1 column (and 1 space) while possible
set background=dark 

if has("gui_running")
    set guioptions-=m           " remove menu bar
    set guioptions-=T           " remove toolbar
    set guioptions-=r           " remove right-hand scroll bar
    set t_Co=256
endif

" show a line at column 79
if exists("&colorcolumn")
    set colorcolumn=79
endif

" displays tabs with :set list & displays when a line runs off-screen
set listchars=tab:>-,eol:$,trail:-,precedes:<,extends:>
set list

"""" Messages, Info, Status
set vb t_vb=                " Disable all bells.  I hate ringing/flashing.
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
set ruler                   " Show some info, even without statuslines.
set laststatus=2            " Always show statusline, even if only 1 window.
"
" Useful information
set statusline=%<%f\ (%{&ft})%=%-19(%3l,%02c%03V%)


"""" Tabs/Indent Levels
set tabstop=4               " <tab> inserts 4 spaces 
set shiftwidth=4            " but an indent level is 2 spaces wide.
set softtabstop=4           " <BS> over an autoindent deletes both spaces.
set expandtab               " Use spaces, not tabs, for autoindent/tab key.
set shiftround              " rounds indent to a multiple of shiftwidth

"""" Tags
" Tags can be in ./tags, ../tags, ..., /home/tags.
"set tags+=$HOME/.vim/tags/python.ctags
"set tags+=$HOME/.vim/tags/django.ctags

set showfulltag             " Show more information while completing tags.
set cscopetag               " When using :tag, <C-]>, or "vim -t", try cscope:
set cscopetagorder=0        " try ":cscope find g foo" and then ":tselect foo"

"""" Reading/Writing
set noautowrite             " Never write a file unless I request it.
set noautowriteall          " NEVER.
set noautoread              " Don't automatically re-read changed files.
set modeline                " Allow vim options to be embedded in files;
set modelines=5             " they must be within the first or last 5 lines.
set ffs=unix,dos,mac        " Try recognizing dos, unix, and mac line endings.

"""" Backups/Swap Files
" Make sure that the directory where we want to put swap/backup files exists.
if has("win32") 
    if ! len(glob("~/backup/"))
        echomsg "Backup directory ~/backup doesn't exist!"
    endif
    set backupdir^=~/backup    " Backups are written to ~/.backup/ if possible.
    set directory^=~/backup//  " Swap files are also written to ~/.backup, too.
endif
if has("unix")
    if ! len(glob("~/.backup/"))
        echomsg "Backup directory ~/.backup doesn't exist!"
    endif
    set backupdir^=~/.backup    " Backups are written to ~/.backup/ if possible.
    set directory^=~/.backup//  " Swap files are also written to ~/.backup, too.
endif

set writebackup             " Make a backup of the original file when writing

"set backup                 " and don't delete it after a succesful write.
set backupskip=             " There are no files that shouldn't be backed up.
set updatetime=2000         " Write swap files after 2 seconds of inactivity.
set backupext=~             " Backup for "file" is "file~"
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

"""" Per-Filetype Scripts
" NOTE: These define autocmds, so they should come before any other autocmds.
"       That way, a later autocmd can override the result of one defined here.
filetype on                 " Enable filetype detection,
filetype indent on          " use filetype-specific indenting where available,
filetype plugin on          " also allow for filetype-specific plugins,
syntax on                   " and turn on per-filetype syntax highlighting.

""" Key Mappings
map <silent><C-Left> <C-T>
map <silent><C-Right> <C-]>

" easily move around tabs
map <silent><A-Right> :tabnext<CR>
map <silent><A-Left> :tabprevious<CR>

" execute selected script
map <C-h> :py EvaluateCurrentRange()<CR>

" Show Project Menu
map <leader>n :NERDTreeToggle<CR>
map <leader>g :call RopeGotoDefinition()<CR>

let g:stop_autocomplete=0

function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction

function! CleverTab(type)
    if a:type=='omni'
        let col = col('.') - 1
        if !col || getline('.')[col - 1] !~ '\k'
            let g:stop_autocomplete=1
            return "\<TAB>"
        elseif !pumvisible() && !&omnifunc
            return "\<C-X>\<C-O>"
        endif
    elseif a:type=='keyword' && !pumvisible() && !g:stop_autocomplete
        return "\<C-X>\<C-N>\<C-P>"
    elseif a:type=='next'
        if g:stop_autocomplete
            let g:stop_autocomplete=0
        else
            return "\<C-N>"
        endif
    endif
    return ''
endfunction

inoremap <silent><TAB> <C-R>=CleverTab('omni')<CR><C-R>=CleverTab('keyword')<CR><C-R>=CleverTab('next')<CR>


" clear the search buffer when hitting return
function! PressedEnter()
    :nohlsearch
    if &filetype == 'python'
        :PyflakesUpdate
    end
endfunction
nnoremap <CR> :nohlsearch\|:call PressedEnter()<cr>

" Seriously, guys. It's not like :W is bound to anything anyway.
command! W :w

let Tlist_GainFocus_On_ToggleOpen=1
let g:skip_loading_mswin=1

" treat html files as django templates
autocmd BufRead *.html set filetype=htmldjango

autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd BufRead *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"      
autocmd BufRead *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m       

if has("python")
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

source ~/.vim/ropevim/rope.vim
endif

"
"""" Display
colorscheme vividchalk
