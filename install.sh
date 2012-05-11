#!/usr/bin/env bash
function link_file {
    source="${PWD}/$1"
    target="${HOME}/${1/_/.}"

    if [ -L "${target}" ]; then
        rm ${target}
    elif [ -e "${target}" ]; then
        mv $target $target.bak
    fi

    ln -sf ${source} ${target}
}

DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

[ -e $DIR/_vim/.vim ] && rm $DIR/_vim/.vim
ln -sf $DIR/_vim $DIR/_vim/.vim

if [ "$1" = "vim" ]; then
    for i in _vim*
    do
       link_file $i
    done
else
    for i in _*
    do
        link_file $i
    done
fi

git submodule sync
git submodule init
git submodule update
git submodule foreach git pull origin master
git submodule foreach git submodule init
git submodule foreach git submodule update

# setup command-t
cd _vim/bundle/command-t
rake make
