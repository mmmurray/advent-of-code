const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');

const parseLine = line => line.split(': ').map(Number);

const initialScanners = input
  .trim()
  .split('\n')
  .map(parseLine)
  .reduce((acc, layer) => {
    const arr = [...acc];
    arr[layer[0]] = { depth: layer[1], position: 0, direction: 1 };
    return arr;
  }, []);

const distance = initialScanners.length;

const cloneScanners = scanners => scanners.map(scanner => ({ ...scanner }));

const updateScanners = scanners => {
  for (let i = 0; i < scanners.length; i++) {
    const scanner = scanners[i];
    if (scanner) {
      if (
        (scanner.position === 0 && scanner.direction === -1) ||
        (scanner.position === scanner.depth - 1 && scanner.direction === 1)
      ) {
        scanner.direction = -scanner.direction;
      }
      scanner.position += scanner.direction;
    }
  }
};

const caughtCrossing = scanners => {
  const cloned = cloneScanners(scanners);
  for (let second = 0; second < distance; second++) {
    const position = second;
    if (cloned[position] && cloned[position].position === 0) {
      return true;
    }

    updateScanners(cloned);
  }
};

let delay = 0;
let currentScanners = initialScanners;
while (true) {
  updateScanners(currentScanners);
  delay++;
  if (!caughtCrossing(currentScanners)) {
    break;
  }
}

console.log(delay);
