const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');

const parseMatches = ([_, name, holding]) => ({ name, holding: holding ? holding.split(', ') : [] });

const parseRow = row => parseMatches(/^([a-z]+) \(\d+\)(?: -> (.+)$)?/.exec(row));

const programs = input
  .trim()
  .split('\n')
  .map(parseRow);

const bottomProgram = programs.find(p1 => !programs.find(p2 => p2.holding.indexOf(p1.name) !== -1));

console.log(bottomProgram.name);
