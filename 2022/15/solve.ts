import { vec2, Vec2, vec2ManhattanDistance } from '@math';

type Reading = {
  sensor: Vec2;
  beacon: Vec2;
  range: number;
};

const parseInput = (input: string): Reading[] => {
  return input
    .trim()
    .split('\n')
    .map<Reading>((line) => {
      const pattern = /x=(-?\d+), y=(-?\d+): .+ x=(-?\d+), y=(-?\d+)/;
      const matches = pattern.exec(line)!;
      const sensor = vec2(Number(matches[1]), Number(matches[2]));
      const beacon = vec2(Number(matches[3]), Number(matches[4]));
      const range = vec2ManhattanDistance(sensor, beacon);

      return { sensor, beacon, range };
    });
};

const getCoveredRanges = (readings: Reading[], y: number): Vec2[] => {
  const ranges: Vec2[] = [];

  for (const { sensor, range } of readings) {
    const yDelta = Math.abs(y - sensor[1]);
    const halfChord = range - yDelta;

    if (halfChord > 0) {
      ranges.push(vec2(sensor[0] - halfChord, sensor[0] + halfChord));
    }
  }

  return ranges.sort((r1, r2) => r1[0] - r2[0]);
};

const findSensorGap = (readings: Reading[], max: number): Vec2 => {
  for (let y = 0; y <= max; y++) {
    const ranges = getCoveredRanges(readings, y);
    let x = 0;

    for (const [xFrom, xTo] of ranges) {
      if (xFrom > x + 1) {
        return vec2(x + 1, y);
      }

      x = Math.max(x, xTo);
    }
  }

  return vec2();
};

export const solvePart1 = (input: string, y: number): number => {
  const readings = parseInput(input);
  const ranges = getCoveredRanges(readings, y);
  let covered = 0;
  let x = ranges[0][0];

  for (const [xFrom, xTo] of ranges) {
    const xNext = Math.max(x, xFrom);
    covered += Math.max(0, xTo - xNext);
    x = Math.max(x, xTo);
  }

  return covered;
};

export const solvePart2 = (input: string, max: number): number => {
  const readings = parseInput(input);
  const sensorGap = findSensorGap(readings, max);

  return sensorGap[0] * 4000000 + sensorGap[1];
};
