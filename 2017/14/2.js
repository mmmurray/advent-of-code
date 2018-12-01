const hash = require('../10/1+2').hash2;

const convertHashToBinary = h =>
  h
    .split('')
    .map(x =>
      parseInt(x, 16)
        .toString(2)
        .padStart(4, '0')
    )
    .join('');

const createGrid = input => {
  const grid = [];
  let total = 0;
  for (let i = 0; i < 128; i++) {
    const hashed = hash(`${input}-${i}`);
    const row = convertHashToBinary(hashed)
      .split('')
      .map(x => (x === '1' ? 1 : 0));

    grid.push(row);
  }
  return grid;
};

const markRegions = (regions, id, x, y) => {
  if (x >= 0 && y >= 0 && x < 128 && y < 128 && typeof regions[y][x] === 'undefined') {
    regions[y][x] = id;
    markRegions(regions, id, x, y - 1);
    markRegions(regions, id, x - 1, y);
    markRegions(regions, id, x + 1, y);
    markRegions(regions, id, x, y + 1);
  }
};

const countRegions = input => {
  let total = 0;
  const grid = createGrid(input);

  const regions = new Array(128).fill().map(x => []);

  let nextRegion = 1;

  for (let y = 0; y < 128; y++) {
    for (let x = 0; x < 128; x++) {
      regions[y][x] = grid[y][x] === 0 ? 0 : undefined;
    }
  }

  for (let y = 0; y < 128; y++) {
    for (let x = 0; x < 128; x++) {
      if (typeof regions[y][x] === 'undefined') {
        markRegions(regions, nextRegion++, x, y);
      }
    }
  }

  return nextRegion - 1;
};

module.exports = countRegions;
