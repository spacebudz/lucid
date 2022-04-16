rm -rf build
mkdir -p build/lucid-cardano
cp -r custom_modules build/lucid-cardano
node ./scripts/build.js

