#!/usr/bin/env bash
for i in _*
do 
    source="${PWD}/$i"
    target="${HOME}/${i/_/.}"
    if [ -e "${target}" ]; then
        echo "${target} already exists"       
    else
        ln -sf ${source} ${target}
    fi
done
