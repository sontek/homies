#!/bin/bash
echo "starting dylib paths"
# echo "crypto"
# CRYPTO_PATH=$(find /nix/store -name libcrypto.dylib|grep openssl-1|head -n1|sed 's/\/lib\/libcrypto.dylib//g')
# echo "openssl"
# OPENSSL_PATH="/nix/store/$(/bin/ls /nix/store|grep openssl-1|grep dev|head -n1)"
MAIN_NIX_PATH=$HOME/.nix-profile
echo "gobject"
GOBJECT_PATH=$(find /nix/store -name "libgobject-2.0.0.dylib" -type f -print -quit|sed 's/\/lib\/libgobject-2.0.0.dylib//g')
echo "pango"
PANGO_PATH=$(find /nix/store -name "libpango-1.0.0.dylib" -type f -print -quit |sed 's/\/lib\/libpango-1.0.0.dylib//g')
echo "harfbuzz"
HARFBUZZ_PATH=$(find /nix/store -name "libharfbuzz.dylib" -type f -print -quit|sed 's/\/lib\/libharfbuzz.dylib//g')
echo "fontconfig"
FONTCONFIG_PATH=$(find /nix/store -name "libfontconfig.dylib" -type f -print -quit|sed 's/\/lib\/libfontconfig.dylib//g')
echo "lzma"
LZMA_PATH=$(find /nix/store -name "lzma.h" -type f -print -quit | sed 's/\/include\/lzma.h//g')
echo "pixman"
PIXMAN_PATH=$(find /nix/store -name "pixman.h" -type f -print -quit | sed 's/\/include\/pixman.h//g')

C_PATHS=("$CRYPTO_PATH" "$OPENSSL_PATH" "$MAIN_NIX_PATH" "$GOBJECT_PATH" "$PANGO_PATH" "$HARFBUZZ_PATH" "$FONTCONFIG_PATH" "$LZMA_PATH", "$PIXMAN_PATH")

echo "done dylib paths"

for p in ${C_PATHS[@]}; do
  DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$p/lib/"
  LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$p/lib/"
  CFLAGS="$CFLAGS -I$p/include/ -I$p/lib/"
  CPPFLAGS="$CPPFLAGS -I$p/include/ -I$p/lib/"
  LDFLAGS="$LDFLAGS -L$p/lib/"
done
echo "starting pkgconfig"
echo "openssl"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name openssl.pc -type f -print -quit| sed 's/openssl.pc//g')"
echo "liblzma"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name liblzma.pc -type f -print -quit| sed 's/liblzma.pc//g')"
echo "pixman"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name pixman-1.pc -type f -print -quit| sed 's/pixman-1.pc//g')"
echo "cairo"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name cairo-ft.pc -type f -print -quit| sed 's/cairo-ft.pc//g')"
echo "libpng"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name libpng16.pc -type f -print -quit| sed 's/libpng16.pc//g')"
echo "pango"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name pangocairo.pc -type f -print -quit| sed 's/pangocairo.pc//g')"
echo "gobject"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name gobject-2.0.pc -type f -print -quit| sed 's/gobject-2.0.pc//g')"
echo "harfbuzz"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name harfbuzz-gobject.pc -type f -print -quit| sed 's/harfbuzz-gobject.pc//g')"
echo "freetype"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name freetype2.pc -type f -print -quit| sed 's/freetype2.pc//g')"
echo "icu-uc"
PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$(find /nix/store -name icu-uc.pc -type f -print -quit| sed 's/icu-uc.pc//g')"
echo "done pkgconfig"

EXPORT_FILE=${XDG_CONFIG_HOME}/zsh/dynamic-exports.zsh
echo "Deleting $EXPORT_FILE"
rm $EXPORT_FILE 
echo "export DYLD_LIBRARY_PATH=\"$DYLD_LIBRARY_PATH\"" >> $EXPORT_FILE
echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\"" >> $EXPORT_FILE
echo "export CFLAGS=\"$CFLAGS\"" >> $EXPORT_FILE
echo "export LDFLAGS=\"$LDFLAGS\"" >> $EXPORT_FILE
echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\"" >> $EXPORT_FILE

