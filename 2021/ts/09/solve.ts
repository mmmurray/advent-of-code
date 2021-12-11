type Grid = number[][]

type Vec2 = [number, number]

type PointSet = {
  add: (point: Vec2) => void
  has: (point: Vec2) => boolean
  size: () => number
}

const createPointSet = (): PointSet => {
  const points = new Map<string, Vec2>()
  return {
    add: (point) => {
      const key = point.join(',')
      points.set(key, point)
    },
    has: (point) => {
      const key = point.join(',')
      return points.get(key) !== undefined
    },
    size: () => {
      return points.size
    },
  }
}

const parseInput = (input: string): Grid => {
  return input
    .trim()
    .split('\n')
    .map((line) => line.split('').map(Number))
}

const getAdjacentCells = (grid: Grid, [x, y]: Vec2): Vec2[] => {
  const adjacentCells: Vec2[] = []
  const height = grid.length
  const width = grid[0].length

  if (x > 0) {
    adjacentCells.push([x - 1, y])
  }
  if (x < width - 1) {
    adjacentCells.push([x + 1, y])
  }
  if (y > 0) {
    adjacentCells.push([x, y - 1])
  }
  if (y < height - 1) {
    adjacentCells.push([x, y + 1])
  }

  return adjacentCells
}

const findLowPoints = (grid: Grid): Vec2[] => {
  const lowPoints: Vec2[] = []
  const height = grid.length
  const width = grid[0].length

  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const point: Vec2 = [x, y]
      const adjacentCells = getAdjacentCells(grid, point)
      let value = grid[y][x]

      if (adjacentCells.every(([x, y]) => grid[y][x] > value)) {
        lowPoints.push(point)
      }
    }
  }

  return lowPoints
}

const basinSize = (grid: Grid, lowPoint: Vec2): number => {
  const basin = createPointSet()
  const targetPoints: Vec2[] = [lowPoint]

  while (targetPoints.length > 0) {
    const targetPoint = targetPoints.shift()!
    const value = grid[targetPoint[1]][targetPoint[0]]
    if (value < 9 && !basin.has(targetPoint)) {
      basin.add(targetPoint)

      getAdjacentCells(grid, targetPoint).forEach((a) => {
        targetPoints.push(a)
      })
    }
  }

  return basin.size()
}

export const solvePart1 = (input: string) => {
  const grid = parseInput(input)
  const lowPoints = findLowPoints(grid)
  const riskLevel = lowPoints.reduce((acc, [x, y]) => acc + grid[y][x] + 1, 0)

  return riskLevel
}

export const solvePart2 = (input: string) => {
  const grid = parseInput(input)
  const lowPoints = findLowPoints(grid)
  const basinSizes = lowPoints.map((lowPoint) => basinSize(grid, lowPoint))
  const [a, b, c] = [...basinSizes].sort((a, b) => b - a)

  return a * b * c
}
