const fs = require('fs');
const {
  dependencies,
  name,
  version,
  description,
  author,
  license,
} = require('../package.json');

const packageJson = {
  module: 'src/index.js',
  type: 'module',
  dependencies,
  name,
  version,
  description,
  author,
  license,
  types: 'src/index.d.ts',
};

fs.writeFileSync(
  './build/lucid-cardano/package.json',
  JSON.stringify(packageJson),
);
