#!/bin/sh
echo "Building project..."
cabal configure
cabal build

echo "Stripping comments from executables..."
strip -p --strip-unneeded --remove-section=.comment -o dikmax-name dist/build/dikmax-name/dikmax-name
strip -p --strip-unneeded --remove-section=.comment -o server dist/build/server/server

echo "Packing archive..."
tar --lzma \
  -cf dikmax-name.tar.lzma dikmax-name server

echo "Transfer archive to server..."
rcp dikmax-name.tar.lzma dikmax.name:/home/dikmax/dikmax.name/dikmax-name.tar.lzma
