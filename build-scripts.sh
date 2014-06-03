#!/bin/sh

echo "Building highlight.js..."
python3.4 js/highlight.js/tools/build.py bash css haskell javascript markdown sql xml dart
cp dart/packages/browser/dart.js dart/s.js
cat dart/packages/browser/interop.js >> dart/s.js
cat js/highlight.js/build/highlight.pack.js >> dart/s.js

echo "Building main dart..."
dart2js --out=dart/script.dart --minify --output-type=dart dart/web/main.dart
echo "Building main js..."
dart2js --out=dart/script.dart.js --minify dart/web/main.dart

echo "Building route-planner dart..."
dart2js --out=dart/script-route-planner.dart --minify --output-type=dart dart/web/route-planner.dart
echo "Building route-planner js..."
dart2js --out=dart/script-route-planner.dart.js --minify dart/web/route-planner.dart

cp dart/packages/browser/dart.js dart/smap.js
cat js/d3/d3.min.js >> dart/smap.js
cat js/topojson/topojson.min.js >> dart/smap.js
cat js/waterman.js >> dart/smap.js

echo "Building map dart..."
dart2js --out=dart/script-map.dart --minify --output-type=dart dart/web/map.dart
echo "Building map js..."
dart2js --out=dart/script-map.dart.js --minify dart/web/map.dart
