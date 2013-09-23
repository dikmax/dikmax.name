#!/bin/sh
echo "Creating backup..."
cp -f dikmax-name dikmax-name.backup
cp -f server server.backup
echo "Extracting..."
tar -xvf dikmax-name.tar.lzma
rm dikmax-name.tar.lzma
echo "Restarting server..."
./dikmax-name rebuild
killall server
nohup ./server &
