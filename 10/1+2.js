const { range, reverse, take, padCharsStart, chunk } = require('lodash/fp');

const flip = (arr, [start, end], flipMiddle) => {
  if (start === end) {
    return arr;
  }

  const startArr = take(start, arr);
  const middleArr = arr.slice(start, end);
  const endArr = arr.slice(end);
  const rev = reverse([...endArr, ...startArr]);

  return flipMiddle
    ? [...startArr, ...reverse(middleArr), ...endArr]
    : [...rev.slice(endArr.length), ...middleArr, ...take(endArr.length, rev)];
};

const getStartEnd = (position, length, size) => {
  const start = position % size;
  const end = (start + length) % size;

  return [Math.min(start, end), Math.max(start, end)];
};

const knot = (size, lengths, initialPosition, initialSkip, initialArr) =>
  lengths.reduce(
    ({ position, skip, arr }, length) => ({
      arr: flip(arr, getStartEnd(position, length, size), position + length < size),
      position: (position + length + skip) % size,
      skip: skip + 1
    }),
    { position: initialPosition, skip: initialSkip, arr: initialArr }
  );

// Part 1

const hash1 = (size, lengths) => {
  const { arr } = knot(size, lengths, 0, 0, range(0, 256));

  return arr[0] * arr[1];
};

// Part 2

const parseInput = input => input.split('').map(c => c.charCodeAt(0));

const xor = arr => arr.reduce((acc, x) => acc ^ x);

const hash2 = input => {
  const lengths = [...parseInput(input), 17, 31, 73, 47, 23];
  const size = 256;
  const rounds = 64;

  const { arr } = range(0, rounds).reduce(
    ({ position, skip, arr }, round) => knot(size, lengths, position, skip, arr),
    { position: 0, skip: 0, arr: range(0, size) }
  );

  return chunk(16, arr)
    .map(xor)
    .map(d => padCharsStart('0', 2, d.toString(16)))
    .join('');
};

module.exports = {
  hash1,
  hash2
};
