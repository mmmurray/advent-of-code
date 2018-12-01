const getLowestBits = num => num - ((num >> 16) << 16);

const getCount = (A, B, rounds) => {
  let count = 0;
  let a = A;
  let b = B;

  for (let i = 0; i < rounds; i++) {
    a = (a * 16807) % 2147483647;
    b = (b * 48271) % 2147483647;

    count += getLowestBits(a) === getLowestBits(b) ? 1 : 0;
  }

  return count;
};

module.exports = getCount;
