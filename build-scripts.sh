#!/bin/sh

echo "Building highlight.js..."
python js/highlight.js/tools/build.py bash css haskell javascript markdown sql xml
cp dart/packages/browser/dart.js dart/s.js
cat dart/packages/browser/interop.js >> dart/s.js
cat js/highlight.js/build/highlight.pack.js >> dart/s.js

echo "Building minified dart..."
dart2js --out=dart/script.dart --minify --output-type=dart dart/main.dart

echo "Building minified js..."
dart2js --out=dart/script.dart.js --minify dart/script.dart
