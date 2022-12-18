import { findShortestPath } from '@algorithms';
import { sum } from '@common';
import {
  vec3,
  Vec3,
  vec3Add,
  vec3Clamp,
  vec3DistanceSquared,
  vec3Max,
} from '@math';

const parseInput = (input: string): Vec3[] => {
  return input
    .trim()
    .split('\n')
    .map((line) => {
      const [x, y, z] = line.split(',').map(Number);
      return vec3(x, y, z);
    });
};

const countExposedSides = (cubes: Vec3[], cube: Vec3): number => {
  const adjacent = cubes.filter(
    (other) => vec3DistanceSquared(cube, other) === 1,
  ).length;

  return 6 - adjacent;
};

const getAdjacentCubes = (cube: Vec3): Vec3[] => {
  return [
    vec3Add(cube, vec3(1, 0, 0)),
    vec3Add(cube, vec3(-1, 0, 0)),
    vec3Add(cube, vec3(0, 1, 0)),
    vec3Add(cube, vec3(0, -1, 0)),
    vec3Add(cube, vec3(0, 0, 1)),
    vec3Add(cube, vec3(0, 0, -1)),
  ];
};

export const solvePart1 = (input: string): number => {
  const cubes = parseInput(input);

  return sum(cubes.map((cube) => countExposedSides(cubes, cube)));
};

export const solvePart2 = (input: string): number => {
  const cubes = parseInput(input);
  const max = vec3Add(cubes.reduce(vec3Max), vec3(1, 1, 1));

  const convertCubeToId = (cube: Vec3): number => {
    return cube[2] * (max[0] * max[1]) + cube[1] * max[0] + cube[0];
  };

  const convertIdToCube = (id: number): Vec3 => {
    const z = Math.floor(id / (max[0] * max[1]));
    const rem = id - z * max[0] * max[1];
    const y = Math.floor(rem / max[0]);
    const x = rem - y * max[0];
    return vec3(x, y, z);
  };

  const outsideCube = vec3(0, 0, 0);
  const outsideCubeId = convertCubeToId(outsideCube);
  const cubeIds = new Set(cubes.map(convertCubeToId));

  const getAdjacentSpaces = (cube: Vec3) =>
    getAdjacentCubes(cube)
      .map((cube) => vec3Clamp(cube, vec3(0, 0, 0), max))
      .filter((adjacent) => !cubeIds.has(convertCubeToId(adjacent)));

  const trappedCubes: Vec3[] = [];
  for (let z = 0; z < max[2]; z++) {
    for (let y = 0; y < max[1]; y++) {
      for (let x = 0; x < max[0]; x++) {
        const cube = vec3(x, y, z);
        const cubeId = convertCubeToId(cube);

        if (cubeIds.has(cubeId)) {
          continue;
        }

        const path = findShortestPath({
          startNode: cubeId,
          targetNode: outsideCubeId,
          getAdjacentNodes: (p) =>
            getAdjacentSpaces(convertIdToCube(p)).map(convertCubeToId),
        });

        if (path === null) {
          trappedCubes.push(cube);
        }
      }
    }
  }

  const totalSurfaceArea = sum(
    cubes.map((cube) => countExposedSides(cubes, cube)),
  );

  const trappedSurfaceArea = sum(
    trappedCubes.map((cube) => countExposedSides(trappedCubes, cube)),
  );

  return totalSurfaceArea - trappedSurfaceArea;
};
