const fs = require('fs');
const {
  dependencies,
  browser,
  name,
  version,
  description,
  author,
  license,
} = require('../package.json');

const packageJson = {
  main: 'src/index.js',
  type: 'module',
  dependencies,
  browser,
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
