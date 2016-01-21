#!/bin/sh

git pull
git submodule update
./dikmax-name build
pub global run static_compress --dir _site
rsync --recursive --delete --force --compress --progress --delay-updates --iconv=UTF8-MAC,UTF-8 _site dikmax@dikmax.name:/home/dikmax/dikmax.name
