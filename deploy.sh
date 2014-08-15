#!/bin/sh

git pull
git submodule update
dist/build/dikmax-name/dikmax-name build
rsync --recursive --delete --force --compress --progress --iconv=UTF8-MAC,UTF-8 _site 188.226.255.34:/home/dikmax/dikmax.name
