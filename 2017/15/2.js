const getLowestBits = num => num - ((num >> 16) << 16);

const getCount = (A, B, rounds) => {
  let count = 0;
  let a = A;
  let b = B;
  let aUsed = 0;
  let bUsed = 0;
  let pairs = 0;
  const judgeA = [];
  const judgeB = [];

  while (pairs < rounds) {
    a = (a * 16807) % 2147483647;
    b = (b * 48271) % 2147483647;

    if (a % 4 === 0) {
      judgeA.push(a);
    }

    if (b % 8 === 0) {
      judgeB.push(b);
    }

    if (aUsed < judgeA.length && bUsed < judgeB.length) {
      const bA = getLowestBits(judgeA[aUsed++]);
      const bB = getLowestBits(judgeB[bUsed++]);

      count += bA === bB ? 1 : 0;
      pairs++;
    }
  }

  return count;
};

module.exports = getCount;
