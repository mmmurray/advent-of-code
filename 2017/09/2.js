const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');
const withoutCancelled = input.replace(/!./g, '');
const matches = withoutCancelled.match(/<[^>]*>/g);
const total = matches.reduce((total, match) => total + match.length - 2, 0);

console.log(total);
