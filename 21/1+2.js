const toString = p => p.join('\n');

const rotate = p =>
  p.length === 2
    ? [`${p[1][0]}${p[0][0]}`, `${p[1][1]}${p[0][1]}`]
    : [`${p[2][0]}${p[1][0]}${p[0][0]}`, `${p[2][1]}${p[1][1]}${p[0][1]}`, `${p[2][2]}${p[1][2]}${p[0][2]}`];

const flip = p =>
  p.length === 3
    ? [`${p[0][2]}${p[0][1]}${p[0][0]}`, `${p[1][2]}${p[1][1]}${p[1][0]}`, `${p[2][2]}${p[2][1]}${p[2][0]}`]
    : p;

const matches = (input, pattern) => {
  const target = toString(pattern);
  const rot1 = rotate(input);
  const rot2 = rotate(rot1);
  const rot3 = rotate(rot2);
  const flip1 = flip(input);
  const flip2 = flip(rot1);
  const flip3 = flip(rot2);
  const flip4 = flip(rot3);
  if (
    toString(input) === target ||
    toString(rot1) === target ||
    toString(rot2) === target ||
    toString(rot3) === target ||
    toString(flip1) === target ||
    toString(flip2) === target ||
    toString(flip3) === target ||
    toString(flip4) === target
  ) {
    return true;
  }

  return false;
};

const subgrid = (grid, chunk, transform) => {
  const newGridSize = grid.length / chunk * (chunk + 1);
  const newGrid = new Array(newGridSize).fill(1).map(_ => new Array(newGridSize).map(x => []));

  for (let y = 0; y < grid.length / chunk; y++) {
    for (let x = 0; x < grid.length / chunk; x++) {
      const chunked = new Array(chunk).fill(1).map(_ => new Array(chunk).map(x => []));
      for (let j = 0; j < chunk; j++) {
        for (let i = 0; i < chunk; i++) {
          chunked[j][i] = grid[y * chunk + j][x * chunk + i];
        }
      }

      const transformed = transform(chunked.map(r => r.join(''))).split('\n');

      for (let j = 0; j < chunk + 1; j++) {
        for (let i = 0; i < chunk + 1; i++) {
          newGrid[y * (chunk + 1) + j][x * (chunk + 1) + i] = transformed[j][i];
        }
      }
    }
  }

  return newGrid.map(r => r.join('')).join('\n');
};

const iterate = (rules, current) => {
  const transform = grid => {
    const rule = rules.find(rule => matches(grid, rule[0]));
    if (rule) {
      return toString(rule[1]);
    }
  };

  if (current.length > 2 && current.length % 2 === 0) {
    return subgrid(current, 2, transform);
  } else if (current.length > 3 && current.length % 3 === 0) {
    return subgrid(current, 3, transform);
  }

  return transform(current);
};

const iterateTimes = (input, times = 1, initial = '.#.\n..#\n###') => {
  const rules = input
    .trim()
    .split('\n')
    .map(l => l.split(' => '))
    .map(l => [l[0].split('/'), l[1].split('/')]);

  let current = initial;

  for (let i = 0; i < times; i++) {
    current = iterate(rules, current.split('\n'));
  }

  return current;
};

const iterateCount = (input, times, initial) => {
  const result = iterateTimes(input, times, initial);
  return result.split('').filter(x => x === '#').length;
};

module.exports = { iterateTimes, iterateCount };
