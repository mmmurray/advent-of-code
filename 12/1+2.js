const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');

const parseLine = line =>
  /^(\d+) <-> (.+)$/
    .exec(line)[2]
    .split(', ')
    .map(Number);

const data = input
  .trim()
  .split('\n')
  .map(parseLine)
  .reduce((acc, linked) => [...acc, linked], []);

const isLinkedTo = (target, id, visited = []) => {
  const linked = data[id];
  visited.push(id);

  if (linked.indexOf(target) !== -1) {
    return true;
  }

  for (let linkedId of linked) {
    if (visited.indexOf(linkedId) === -1 && isLinkedTo(target, linkedId, visited)) {
      return true;
    }
  }

  return false;
};

// Part 1
const linkedToZero = data.filter((_, index) => isLinkedTo(0, index)).length;
console.log(linkedToZero);

// Part 2
const groups = [];

data.forEach((_, index) => {
  for (let target of groups) {
    if (isLinkedTo(target, index)) {
      return;
    }
  }
  groups.push(index);
});

const numberOfGroups = Object.keys(groups).length;
console.log(numberOfGroups);
