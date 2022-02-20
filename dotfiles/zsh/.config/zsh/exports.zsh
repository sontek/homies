# Setup the environment to have ASDF controlled
# binaries be that the fron of the path
if command -v asdf &> /dev/null
then
    ASDF_PATH=$(asdf info|grep ASDF_DIR|awk -F'=' '{ print $2}')
    . $ASDF_PATH/asdf.sh
    . $ASDF_PATH/completions/asdf.bash
fi


# Enable completions when using kubectl
if command -v kubectl &> /dev/null
then
    . <(kubectl completion zsh)
    complete -F __start_kubectl k
fi

export DYLD_LIBRARY_PATH=$HOME/.nix-profile/lib/


