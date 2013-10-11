#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${DIR}
git pull
cd disqus-sync
node sync.js
cd ..
git add comments/*.html
git commit -m "Comments sync"
git push
