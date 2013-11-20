#!/bin/sh
git pull
git submodule update
./dikmax-name build
echo "Done!"
