const fs = require('fs');
const package = require('../package.json');
delete package.type;
fs.writeFileSync('package.json', JSON.stringify(package, null, 2));
