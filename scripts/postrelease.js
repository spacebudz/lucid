import fs from 'fs';
const p = JSON.parse(fs.readFileSync('package.json', 'utf-8'));
delete p.type;
fs.writeFileSync('package.json', JSON.stringify(p, null, 2));
fs.appendFileSync('package.json', '\n');
