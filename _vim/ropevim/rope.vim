let $PYTHONPATH .= ":/Users/johnanderson/code/dotfiles/_vim/ropevim/pylibs"
python << EOF
sys.path.append("/Users/johnanderson/code/dotfiles/_vim/ropevim/pylibs")
EOF
source /Users/johnanderson/code/dotfiles/_vim/ropevim/src/ropevim/ropevim.vim
