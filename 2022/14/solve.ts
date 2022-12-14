import {
  vec2,
  Vec2,
  vec2Add,
  vec2Equal,
  vec2Max,
  vec2Min,
  vec2Normalize,
  vec2Sub,
} from '@math';

type SparseGrid = {
  has: (point: Vec2) => boolean;
  add: (point: Vec2) => void;
  delete: (point: Vec2) => void;
};

const createSparseGrid = (): SparseGrid => {
  const cells = new Map<number, Set<number>>();

  return {
    has: ([x, y]) => {
      const row = cells.get(y);
      return row !== undefined && row.has(x);
    },
    add: ([x, y]) => {
      const row = cells.get(y) ?? new Set();
      row.add(x);
      cells.set(y, row);
    },
    delete: ([x, y]) => {
      const row = cells.get(y) ?? new Set();
      row.delete(x);
      cells.set(y, row);
    },
  };
};

type Cave = {
  walls: SparseGrid;
  minPoint: Vec2;
  maxPoint: Vec2;
};

const sandOrigin = vec2(500, 0);

const parseInput = (input: string): Cave => {
  const wallPaths = input
    .trim()
    .split('\n')
    .map((line) => {
      return line.split(' -> ').map((point) => {
        const [x, y] = point.split(',').map(Number);
        return vec2(x, y);
      });
    });

  let minPoint = sandOrigin;
  let maxPoint = sandOrigin;
  const walls = createSparseGrid();

  const placeWall = (point: Vec2) => {
    walls.add(point);
    minPoint = vec2Min(minPoint, point);
    maxPoint = vec2Max(maxPoint, point);
  };

  wallPaths.forEach((wall) => {
    for (let i = 1; i < wall.length; i++) {
      const from = wall[i - 1];
      const to = wall[i];
      const delta = vec2Normalize(vec2Sub(to, from));

      let current = from;
      placeWall(current);
      while (!vec2Equal(current, to)) {
        current = vec2Add(current, delta);
        placeWall(current);
      }
    }
  });

  return { walls, minPoint, maxPoint };
};

const debug = (cave: Cave, sand: SparseGrid) => {
  const lines: string[] = [];
  for (let y = cave.minPoint[1]; y <= cave.maxPoint[1]; y++) {
    let line = '';
    for (let x = cave.minPoint[0]; x <= cave.maxPoint[0]; x++) {
      let cell = '.';
      const point = vec2(x, y);
      if (cave.walls.has(point)) {
        cell = '#';
      } else if (sand.has(point)) {
        cell = 'o';
      }
      line += cell;
    }
    lines.push(line);
  }
  const output = lines.join('\n');
  console.log(output);
};

const updateSand = (
  cave: Cave,
  sand: SparseGrid,
  current: Vec2,
): Vec2 | null => {
  const next1 = vec2Add(current, vec2(0, 1));
  if (!cave.walls.has(next1) && !sand.has(next1)) {
    return next1;
  }
  const next2 = vec2Add(current, vec2(-1, 1));
  if (!cave.walls.has(next2) && !sand.has(next2)) {
    return next2;
  }
  const next3 = vec2Add(current, vec2(1, 1));
  if (!cave.walls.has(next3) && !sand.has(next3)) {
    return next3;
  }

  return null;
};

const dropSand = (cave: Cave, sand: SparseGrid): Vec2 => {
  let current = sandOrigin;
  while (true) {
    if (current[1] === cave.maxPoint[1] + 1) {
      break;
    }

    const next = updateSand(cave, sand, current);
    if (next) {
      current = next;
    } else {
      break;
    }
  }

  sand.add(current);

  return current;
};

const solve = (
  input: string,
  settleCondition: (point: Vec2, cave: Cave) => boolean,
): number => {
  const cave = parseInput(input);
  const sand = createSparseGrid();
  let dropped = 0;

  while (!settleCondition(dropSand(cave, sand), cave)) {
    dropped++;
  }

  return dropped;
};

export const solvePart1 = (input: string): number => {
  return solve(input, (point, cave) => point[1] >= cave.maxPoint[1]);
};

export const solvePart2 = (input: string): number => {
  return solve(input, (point) => vec2Equal(point, sandOrigin)) + 1;
};
