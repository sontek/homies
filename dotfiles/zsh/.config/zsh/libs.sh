#!/bin/bash
CRYPTO_PATH=$(find /nix/store -name libcrypto.dylib|grep openssl-1|head -n1|sed 's/\/lib\/libcrypto.dylib//g')
OPENSSL_PATH="/nix/store/$(/bin/ls /nix/store|grep openssl-1|grep dev|head -n1)"
MAIN_NIX_PATH=$HOME/.nix-profile
GOBJECT_PATH=$(find /nix/store -name "libgobject-2.0.0.dylib" |head -n1|sed 's/\/lib\/libgobject-2.0.0.dylib//g')
PANGO_PATH=$(find /nix/store -name "libpango-1.0.0.dylib" |head -n1|sed 's/\/lib\/libpango-1.0.0.dylib//g')
HARFBUZZ_PATH=$(find /nix/store -name "libharfbuzz.dylib" |head -n1|sed 's/\/lib\/libharfbuzz.dylib//g')
FONTCONFIG_PATH=$(find /nix/store -name "libfontconfig.dylib" |head -n1|sed 's/\/lib\/libfontconfig.dylib//g')
LZMA_PATH=$(find /nix/store -name "lzma.h" | head -n1 | sed 's/\/include\/lzma.h//g')
PIXMAN_PATH=$(find /nix/store -name "pixman.h" | head -n1 | sed 's/\/include\/pixman.h//g')

C_PATHS=("$CRYPTO_PATH" "$OPENSSL_PATH" "$MAIN_NIX_PATH" "$GOBJECT_PATH" "$PANGO_PATH" "$HARFBUZZ_PATH" "$FONTCONFIG_PATH" "$LZMA_PATH", "$PIXMAN_PATH")


for p in ${C_PATHS[@]}; do
  DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$p/lib/"
  LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$p/lib/"
  CFLAGS="$CFLAGS -I$p/include/ -I$p/lib/"
  CPPFLAGS="$CPPFLAGS -I$p/include/ -I$p/lib/"
  LDFLAGS="$LDFLAGS -L$p/lib/"
done

PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name openssl.pc|head -n1| sed 's/openssl.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name liblzma.pc|head -n1| sed 's/liblzma.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name pixman-1.pc|head -n1| sed 's/pixman-1.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name cairo-ft.pc|head -n1| sed 's/cairo-ft.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name libpng16.pc|head -n1| sed 's/libpng16.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name pangocairo.pc|head -n1| sed 's/pangocairo.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name gobject-2.0.pc|head -n1| sed 's/gobject-2.0.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name harfbuzz-gobject.pc|head -n1| sed 's/harfbuzz-gobject.pc//g')"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name freetype2.pc|head -n1| sed 's/freetype2.pc//g')"

EXPORT_FILE=${XDG_CONFIG_HOME}/zsh/dynamic-exports.zsh
echo "Deleting $EXPORT_FILE"
rm $EXPORT_FILE 
echo "export DYLD_LIBRARY_PATH=\"$DYLD_LIBRARY_PATH\"" >> $EXPORT_FILE
echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\"" >> $EXPORT_FILE
echo "export CFLAGS=\"$CFLAGS\"" >> $EXPORT_FILE
echo "export LDFLAGS=\"$LDFLAGS\"" >> $EXPORT_FILE
echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\"" >> $EXPORT_FILE

