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
git submodule status | awk '/^-/ { print $2 }' | xargs -r git submodule update
git submodule foreach git pull origin master
git submodule foreach git submodule init
git submodule foreach git submodule update

#cd _vim/ropevim/
#./install.sh
