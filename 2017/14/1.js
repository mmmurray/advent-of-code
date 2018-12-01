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

const countSquares = input => {
  let total = 0;
  for (let i = 0; i < 128; i++) {
    const hashed = hash(`${input}-${i}`);
    const used = convertHashToBinary(hashed)
      .split('')
      .filter(x => x === '1').length;
    total += used;
  }
  return total;
};

module.exports = countSquares;
