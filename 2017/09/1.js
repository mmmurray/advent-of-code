const removeCancelled = x => x.replace(/!./g, '');
const removeGarbage = x => x.replace(/<[^>]*>/g, '');
const squigglyToSquare = x => x.replace(/{/g, '[').replace(/}/g, ']');
const count = (arr, level = 1) => level + arr.reduce((total, subArr) => total + count(subArr, level + 1), 0);

const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');
const total = count(eval(squigglyToSquare(removeGarbage(removeCancelled(input)))));

console.log(total);
