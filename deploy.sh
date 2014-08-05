#!/bin/sh

rsync --recursive --verbose --delete --force --compress _site 188.226.255.34:/home/dikmax/dikmax.name
