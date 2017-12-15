const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');

const parseLine = line => line.split(': ').map(Number);

const scanners = input
  .trim()
  .split('\n')
  .map(parseLine)
  .reduce((acc, layer) => {
    const arr = [...acc];
    arr[layer[0]] = { depth: layer[1], position: 0, direction: 1 };
    return arr;
  }, []);

const activeScanners = scanners.filter(Boolean);

const updateScanners = () => {
  for (let i = 0; i < activeScanners.length; i++) {
    const scanner = activeScanners[i];
    if (
      (scanner.position === 0 && scanner.direction === -1) ||
      (scanner.position === scanner.depth - 1 && scanner.direction === 1)
    ) {
      scanner.direction = -scanner.direction;
    }
    scanner.position += scanner.direction;
  }
};

let totalSeverity = 0;

for (let second = 0; second < scanners.length; second++) {
  const position = second;
  if (scanners[position] && scanners[position].position === 0) {
    totalSeverity += position * scanners[position].depth;
  }

  updateScanners();
}

console.log(totalSeverity);
