#!/bin/sh

git pull
git submodule update
dist/build/dikmax-name/dikmax-name build
pub global run static_compress --dir _site
rsync --recursive --delete --force --compress --progress --iconv=UTF8-MAC,UTF-8 _site dikmax.name:/home/dikmax/dikmax.name
