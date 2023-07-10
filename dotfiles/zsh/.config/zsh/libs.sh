#!/bin/bash
CRYPTO_PATH=$(find /nix/store -name libcrypto.dylib|grep openssl-1|head -n1|sed 's/\/lib\/libcrypto.dylib//g')
OPENSSL_PATH="/nix/store/$(/bin/ls /nix/store|grep openssl-1|grep dev|head -n1)"
MAIN_NIX_PATH=$HOME/.nix-profile
GOBJECT_PATH=$(find /nix/store -name "libgobject-2.0.0.dylib" |head -n1|sed 's/\/lib\/libgobject-2.0.0.dylib//g')
PANGO_PATH=$(find /nix/store -name "libpango-1.0.0.dylib" |head -n1|sed 's/\/lib\/libpango-1.0.0.dylib//g')
HARFBUZZ_PATH=$(find /nix/store -name "libharfbuzz.dylib" |head -n1|sed 's/\/lib\/libharfbuzz.dylib//g')
FONTCONFIG_PATH=$(find /nix/store -name "libfontconfig.dylib" |head -n1|sed 's/\/lib\/libfontconfig.dylib//g')
C_PATHS=("$CRYPTO_PATH" "$OPENSSL_PATH" "$MAIN_NIX_PATH" "$GOBJECT_PATH" "$PANGO_PATH" "$HARFBUZZ_PATH" "$FONTCONFIG_PATH")


for p in ${C_PATHS[@]}; do
  DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$p/lib/"
  LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$p/lib/"
  CFLAGS="$CFLAGS -I$p/include/ -I$p/lib/"
  CPPFLAGS="$CPPFLAGS -I$p/include/ -I$p/lib/"
  LDFLAGS="$LDFLAGS -L$p/lib/"
done

PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name openssl.pc|head -n1| sed 's/openssl.pc//g')"
EXPORT_FILE=${XDG_CONFIG_HOME}/zsh/dynamic-exports.zsh
echo "Deleting $EXPORT_FILE"
rm $EXPORT_FILE 
echo "export DYLD_LIBRARY_PATH=\"$DYLD_LIBRARY_PATH\"" >> $EXPORT_FILE
echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\"" >> $EXPORT_FILE
echo "export CFLAGS=\"$CFLAGS\"" >> $EXPORT_FILE
echo "export LDFLAGS=\"$LDFLAGS\"" >> $EXPORT_FILE
echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\"" >> $EXPORT_FILE

