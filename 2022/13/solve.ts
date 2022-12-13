import { deepEqual, product, sum } from '@common';

type Packet = number | Packet[];

const parseInput = (input: string): [Packet, Packet][] => {
  const pairs = input.trim().split('\n\n');

  return pairs.map((pair) => {
    const [a, b] = pair.split('\n');
    return [JSON.parse(a), JSON.parse(b)];
  });
};

const comparePackets = (a: Packet, b: Packet): -1 | 0 | 1 => {
  if (typeof a === 'number' && typeof b === 'number') {
    if (a === b) {
      return 0;
    }
    return a < b ? -1 : 1;
  }
  if (typeof a === 'number') {
    return comparePackets([a], b);
  }
  if (typeof b === 'number') {
    return comparePackets(a, [b]);
  }

  if (Array.isArray(a) && Array.isArray(b)) {
    const as = [...a];
    const bs = [...b];

    while (true) {
      const a = as.shift();
      const b = bs.shift();

      if (a === undefined && b === undefined) {
        return 0;
      }
      if (a === undefined) {
        return -1;
      }
      if (b === undefined) {
        return 1;
      }

      const result = comparePackets(a, b);
      if (result !== 0) {
        return result;
      }
    }
  }

  return 1;
};

export const solvePart1 = (input: string): number => {
  const pairs = parseInput(input);

  return sum(
    pairs.map(([a, b], index) => (comparePackets(a, b) === -1 ? index + 1 : 0)),
  );
};

export const solvePart2 = (input: string): number => {
  const pairs = parseInput(input);
  const dividerPackets: Packet[] = [[[2]], [[6]]];
  const allPackets = [...pairs.flat(), ...dividerPackets];
  const sortedPackets = allPackets.sort(comparePackets);

  const dividerPacketsIndices = dividerPackets.map(
    (dividerPacket) =>
      sortedPackets.findIndex((other) => deepEqual(other, dividerPacket)) + 1,
  );

  return product(dividerPacketsIndices);
};
