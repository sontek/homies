#!/usr/bin/env bash
for i in _*
do 
    source="${PWD}/$i"
    target="${HOME}/${i/_/.}"
#    if [ -e "${target}" ]; then
#        echo "${target} already exists"       
#    else
        ln -sf ${source} ${target}
#    fi
done

git submodule sync
git submodule init
git submodule update
git submodule foreach git pull origin master
git submodule foreach git submodule init
git submodule foreach git submodule update

# setup command-t
cd _vim/bundle/command-t
rake make

#cd _vim/ropevim/
#./install.sh
