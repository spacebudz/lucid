const fs = require('fs');
const package = require('../package.json');
package.type = 'module';
fs.writeFileSync('package.json', JSON.stringify(package, null, 2));
