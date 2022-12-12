import { findShortestPath } from '@algorithms';
import { vec2, Vec2 } from '@math';

type HeightMap = {
  cols: number;
  rows: number;
  elevations: number[][];
  start: Vec2;
  end: Vec2;
};

const getCharElevation = (char: string): number => char.charCodeAt(0) - 97;

const parseInput = (input: string): HeightMap => {
  const lines = input.trim().split('\n');
  const elevations: number[][] = [];
  let start = vec2();
  let end = vec2();

  for (const line of lines) {
    const row: number[] = [];
    const y = elevations.length;
    line.split('').forEach((char, x) => {
      let height: number;
      if (char === 'S') {
        start = vec2(x, y);
        height = getCharElevation('a');
      } else if (char === 'E') {
        end = vec2(x, y);
        height = getCharElevation('z');
      } else {
        height = getCharElevation(char);
      }
      row.push(height);
    });
    elevations.push(row);
  }

  const cols = elevations[0].length;
  const rows = elevations.length;

  return { cols, rows, elevations, start, end };
};

const cellToIndex = (heightMap: HeightMap, cell: Vec2): number =>
  cell[1] * heightMap.cols + cell[0];

const indexToCell = (heightMap: HeightMap, index: number): Vec2 => {
  const y = Math.floor(index / heightMap.cols);
  const x = index - y * heightMap.cols;

  return vec2(x, y);
};

const getAjacentCells = (heightMap: HeightMap, cell: Vec2): Vec2[] => {
  const adjacentCells: Vec2[] = [];

  if (cell[0] > 0) {
    adjacentCells.push(vec2(cell[0] - 1, cell[1]));
  }
  if (cell[0] < heightMap.cols - 1) {
    adjacentCells.push(vec2(cell[0] + 1, cell[1]));
  }
  if (cell[1] > 0) {
    adjacentCells.push(vec2(cell[0], cell[1] - 1));
  }
  if (cell[1] < heightMap.rows - 1) {
    adjacentCells.push(vec2(cell[0], cell[1] + 1));
  }

  return adjacentCells;
};

const getAvailableCells = (heightMap: HeightMap, cell: Vec2): Vec2[] => {
  const currentElevation = heightMap.elevations[cell[1]][cell[0]];
  const adjacentCells = getAjacentCells(heightMap, cell);

  return adjacentCells.filter((adjacentCell) => {
    const elevation = heightMap.elevations[adjacentCell[1]][adjacentCell[0]];
    return elevation <= currentElevation + 1;
  });
};

const getShortestTrailLength = (heightMap: HeightMap): number => {
  const shortestPath = findShortestPath({
    startNode: cellToIndex(heightMap, heightMap.start),
    targetNode: cellToIndex(heightMap, heightMap.end),
    getAdjacentNodes: (index) =>
      getAvailableCells(heightMap, indexToCell(heightMap, index)).map((cell) =>
        cellToIndex(heightMap, cell),
      ),
  });

  return shortestPath ? shortestPath.length - 1 : Infinity;
};

export const solvePart1 = (input: string): number => {
  const heightMap = parseInput(input);

  return getShortestTrailLength(heightMap);
};

export const solvePart2 = (input: string): number => {
  const heightMap = parseInput(input);
  let minTrailLength = Infinity;

  for (let y = 0; y < heightMap.rows; y++) {
    for (let x = 0; x < heightMap.cols; x++) {
      if (heightMap.elevations[y][x] === 0) {
        minTrailLength = Math.min(
          minTrailLength,
          getShortestTrailLength({ ...heightMap, start: vec2(x, y) }),
        );
      }
    }
  }

  return minTrailLength;
};
