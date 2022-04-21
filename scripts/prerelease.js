const fs = require('fs');
const p = JSON.parse(fs.readFileSync('package.json', 'utf-8'));
p.type = 'module';
fs.writeFileSync('package.json', JSON.stringify(p, null, 2));
