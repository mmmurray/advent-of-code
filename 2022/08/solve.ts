import { vec2, Vec2, vec2Add } from '@math'

type Grid = {
  width: number
  height: number
  cells: number[][]
}

const parseInput = (input: string): Grid => {
  const cells = input
    .trim()
    .split('\n')
    .map((line) => {
      return line.split('').map(Number)
    })

  const width = cells[0].length
  const height = cells.length

  return { width, height, cells }
}

const gridContains = (grid: Grid, [x, y]: Vec2): boolean => {
  return x >= 0 && y >= 0 && x < grid.width && y < grid.height
}

const getTreeHeight = (grid: Grid, [x, y]: Vec2): number => {
  return grid.cells[y][x]
}

const getTreesInDirection = (
  grid: Grid,
  direction: Vec2,
  from: Vec2,
): Vec2[] => {
  const next = vec2Add(from, direction)

  return gridContains(grid, next)
    ? [next, ...getTreesInDirection(grid, direction, next)]
    : []
}

const isTreeVisibleFromOutsideGrid = (grid: Grid, tree: Vec2): boolean => {
  const treeHeight = getTreeHeight(grid, tree)

  const treesInLineOfSight = [
    getTreesInDirection(grid, vec2(0, -1), tree),
    getTreesInDirection(grid, vec2(0, 1), tree),
    getTreesInDirection(grid, vec2(-1, 0), tree),
    getTreesInDirection(grid, vec2(1, 0), tree),
  ]

  return treesInLineOfSight.some((path) =>
    path.every((other) => getTreeHeight(grid, other) < treeHeight),
  )
}

const getScenicScore = (grid: Grid, tree: Vec2): number => {
  const treeHeight = getTreeHeight(grid, tree)

  const treesInLineOfSight = [
    getTreesInDirection(grid, vec2(0, -1), tree),
    getTreesInDirection(grid, vec2(-1, 0), tree),
    getTreesInDirection(grid, vec2(1, 0), tree),
    getTreesInDirection(grid, vec2(0, 1), tree),
  ]

  return treesInLineOfSight.reduce((acc, trees) => {
    let distance = 0
    for (const other of trees) {
      distance += 1

      if (getTreeHeight(grid, other) >= treeHeight) {
        break
      }
    }

    return acc * distance
  }, 1)
}

export const solvePart1 = (input: string): number => {
  const grid = parseInput(input)

  let visibleCount = 0
  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      if (isTreeVisibleFromOutsideGrid(grid, vec2(x, y))) {
        visibleCount++
      }
    }
  }

  return visibleCount
}

export const solvePart2 = (input: string): number => {
  const grid = parseInput(input)

  let maxScenicScore = 0
  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      const scenicScore = getScenicScore(grid, vec2(x, y))
      maxScenicScore = Math.max(maxScenicScore, scenicScore)
    }
  }

  return maxScenicScore
}
