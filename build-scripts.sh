#!/bin/sh

# Uncomment only if required
echo "Building highlight.js..."
python js/highlight.js/tools/build.py bash css haskell javascript markdown sql xml
cp dart/packages/browser/dart.js dart/s.js
cat dart/packages/browser/interop.js >> dart/s.js
cat js/highlight.js/build/highlight.pack.js >> dart/s.js

echo "Building minified dart..."
dart2js --out=dart/script.dart --minify --output-type=dart dart/main.dart

echo "Building minified js..."
dart2js --out=dart/script.dart.js --minify dart/script.dart

#echo "Building templates..."
#for file in $(find js/dikmax -iname "*.soy")
#do
#  echo $file
#  java -jar js/closure-templates/SoyToJsSrcCompiler.jar --shouldGenerateJsdoc --shouldProvideRequireSoyNamespaces --outputPathFormat {INPUT_DIRECTORY}/{INPUT_FILE_NAME_NO_EXT}.js $file
#done

#echo "Updating dependencies..."
#python js/closure-library/closure/bin/build/depswriter.py \
#  --root_with_prefix="js/closure-library/third_party/closure/goog ../../third_party/closure/goog" \
#  --root_with_prefix="js/dikmax ../../../dikmax" \
#  --root=js/closure-library/closure/goog \
#  --path_with_depspath="js/closure-templates/soyutils_usegoog.js ../../../closure-templates/soyutils_usegoog.js" \
#  --path_with_depspath="js/highlight.pack.js ../../../highlight.pack.js" \
#  --output_file=js/deps.js

#echo "Compiling script..."
#python js/closure-library/closure/bin/build/closurebuilder.py \
#  --root=js/dikmax/ \
#  --root=js/closure-library/closure/goog/ \
#  --root=js/closure-library/third_party/closure/goog/ \
#  --namespace="dikmax.main" \
#  --output_mode=compiled \
#  --compiler_jar=js/closure-compiler/compiler.jar \
#  --compiler_flags="--compilation_level=ADVANCED_OPTIMIZATIONS" \
#  --compiler_flags="--use_types_for_optimization" \
#  --compiler_flags="--warning_level=VERBOSE" \
#  --compiler_flags="--charset=UTF-8" \
#  --compiler_flags="--define='goog.LOCALE=\"ru\"'" \
#  --compiler_flags="--create_source_map=js/script.js.map" \
#  js/closure-templates/soyutils_usegoog.js \
#  js/highlight.pack.js \
#  > js/script.js
