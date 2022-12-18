import { vec2, Vec2, vec2Add } from '@math';

const chamberWidth = 7;

const parseInput = (input: string): number[] => {
  return input
    .trim()
    .split('')
    .map((char) => (char === '>' ? 1 : -1));
};

type Shape = {
  size: Vec2;
  cells: Vec2[];
};

const shapes: Shape[] = [
  // ####
  {
    size: vec2(4, 1),
    cells: [vec2(0, 0), vec2(1, 0), vec2(2, 0), vec2(3, 0)],
  },

  // .#.
  // ###
  // .#.
  {
    size: vec2(3, 3),
    cells: [vec2(1, 2), vec2(0, 1), vec2(1, 1), vec2(2, 1), vec2(1, 0)],
  },

  // ..#
  // ..#
  // ###
  {
    size: vec2(3, 3),
    cells: [vec2(2, 2), vec2(2, 1), vec2(0, 0), vec2(1, 0), vec2(2, 0)],
  },

  // #
  // #
  // #
  // #
  {
    size: vec2(1, 4),
    cells: [vec2(0, 3), vec2(0, 2), vec2(0, 1), vec2(0, 0)],
  },

  // ##
  // ##
  {
    size: vec2(2, 2),
    cells: [vec2(0, 1), vec2(1, 1), vec2(0, 0), vec2(1, 0)],
  },
];

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

const placeShape = (grid: SparseGrid, shape: Shape, position: Vec2): number => {
  let maxY = 0;

  shape.cells.forEach((cell) => {
    const p = vec2Add(cell, position);
    maxY = Math.max(maxY, p[1]);
    grid.add(p);
  });

  return maxY + 1;
};

const shapePositionValid = (grid: SparseGrid, shape: Shape, position: Vec2) => {
  const outOfBounds = shape.cells.some((cell) => {
    const p = vec2Add(cell, position);

    return p[0] < 0 || p[0] >= chamberWidth || p[1] < 0;
  });

  if (outOfBounds) {
    return false;
  }

  return !shape.cells.some((cell) => grid.has(vec2Add(cell, position)));
};

const getGridHeights = (grid: SparseGrid, maxY: number): number[] => {
  const heights: number[] = [];
  for (let x = 0; x < chamberWidth; x++) {
    let y = maxY;
    while (!grid.has(vec2(x, y)) && y > 0) {
      y -= 1;
    }

    heights.push(maxY - y);
  }

  return heights;
};

const dropShape = ({
  stream,
  grid,
  time,
  shapeIndex,
  maxY,
}: {
  stream: number[];
  grid: SparseGrid;
  time: number;
  shapeIndex: number;
  maxY: number;
}) => {
  const shape = shapes[shapeIndex];
  let position = vec2(2, maxY + 3);

  while (true) {
    const jetIndex = time++ % stream.length;
    const jet = stream[jetIndex];
    const jetPosition = vec2Add(position, vec2(jet, 0));

    if (shapePositionValid(grid, shape, jetPosition)) {
      position = jetPosition;
    }

    const fallPosition = vec2Add(position, vec2(0, -1));

    if (shapePositionValid(grid, shape, fallPosition)) {
      position = fallPosition;
    } else {
      maxY = Math.max(maxY, placeShape(grid, shape, position));
      return { jetIndex, time, maxY };
    }
  }
};

export const solvePart1 = (input: string): number => {
  const stream = parseInput(input);
  const grid = createSparseGrid();
  let maxY = 0;
  let time = 0;
  let placed = 0;
  const toPlace = 2022;

  while (placed < toPlace) {
    const shapeIndex = placed % shapes.length;
    const result = dropShape({
      stream,
      grid,
      time,
      shapeIndex,
      maxY,
    });

    maxY = result.maxY;
    time = result.time;
    placed++;
  }

  return maxY;
};

export const solvePart2 = (input: string): number => {
  const stream = parseInput(input);
  const grid = createSparseGrid();
  let maxY = 0;
  let time = 0;
  let placed = 0;
  const toPlace = 1000000000000;
  let prevStates: { [key in string]: { maxY: number; placed: number } } = {};
  let additionalHeight = 0;

  while (placed < toPlace) {
    const shapeIndex = placed % shapes.length;
    const result = dropShape({
      stream,
      grid,
      time,
      shapeIndex,
      maxY,
    });

    maxY = result.maxY;
    time = result.time;
    placed++;

    const stateKey = [result.jetIndex, ...getGridHeights(grid, maxY)].join(',');
    const prevState = prevStates[stateKey];
    if (prevState && additionalHeight === 0) {
      const cycleHeightDelta = maxY - prevState.maxY;
      const cyclePlacedDelta = placed - prevState.placed;
      const cycles = Math.floor((toPlace - placed) / cyclePlacedDelta);
      placed += cycles * cyclePlacedDelta;
      additionalHeight = cycles * cycleHeightDelta;
    } else {
      prevStates[stateKey] = { maxY, placed };
    }
  }

  return maxY + additionalHeight;
};
