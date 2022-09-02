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

# Enable completions when using aws
complete -C 'aws_completer' aws

# Load Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

export CRYPTO_PATH=$(find /nix/store -name libcrypto.dylib|grep openssl|head -n1|sed 's/\/lib\/libcrypto.dylib//g')
export OPENSSL_PATH="/nix/store/$(/bin/ls /nix/store|grep openssl|grep dev|head -n1)"
export MAIN_NIX_PATH=$HOME/.nix-profile
export GOBJECT_PATH=$(find /nix/store -name "libgobject-2.0.0.dylib" |head -n1|sed 's/\/lib\/libgobject-2.0.0.dylib//g')
export PANGO_PATH=$(find /nix/store -name "libpango-1.0.0.dylib" |head -n1|sed 's/\/lib\/libpango-1.0.0.dylib//g')
export HARFBUZZ_PATH=$(find /nix/store -name "libharfbuzz.dylib" |head -n1|sed 's/\/lib\/libharfbuzz.dylib//g')
export FONTCONFIG_PATH=$(find /nix/store -name "libfontconfig.dylib" |head -n1|sed 's/\/lib\/libfontconfig.dylib//g')
C_PATHS=("$CRYPTO_PATH" "$OPENSSL_PATH" "$MAIN_NIX_PATH" "$GOBJECT_PATH" "$PANGO_PATH" "$HARFBUZZ_PATH" "$FONTCONFIG_PATH")


for p in ${C_PATHS[@]}; do
  export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$p/lib/"
  export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$p/lib/"
  export CFLAGS="$CFLAGS -I$p/include/"
  export CPPFLAGS="$CPPFLAGS -I$p/include/"
  export LDFLAGS="$LDFLAGS -L$p/lib/"
done

# Add Nix to $PATH
export PATH=$HOME/.nix-profile/bin:$PATH

# Add krew to $PATH
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# Add rust to the $PATH
export PATH="$HOME/.cargo/bin:$PATH"

eval "$(direnv hook zsh)"

# Use ipdb by default when debugging python
export PYTHONBREAKPOINT=ipdb.set_trace
