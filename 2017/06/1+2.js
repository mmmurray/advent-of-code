const memory = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6];
const seen = [];

const serialise = () => memory.join(',');

while (seen.indexOf(serialise()) === -1) {
  seen.push(serialise());
  let redistribute = Math.max(...memory);
  let index = memory.findIndex(x => x === redistribute);
  memory[index] = 0;
  while (redistribute > 0) {
    index = (index + 1) % memory.length;
    memory[index] += 1;
    redistribute -= 1;
  }
}

console.log(`Redistributions: ${seen.length}`);
console.log(`Cycle length: ${seen.length - seen.indexOf(serialise())}`);
