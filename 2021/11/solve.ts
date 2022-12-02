type Grid = number[][]

type Vec2 = [number, number]

type PointSet = {
  add: (point: Vec2) => void
  has: (point: Vec2) => boolean
  size: () => number
  values: () => Vec2[]
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
    values: () => {
      return Array.from(points.values())
    },
  }
}

const parseInput = (input: string): Grid => {
  return input
    .trim()
    .split('\n')
    .map((line) => line.split('').map(Number))
}

const width = 10
const height = 10

const step = (grid: Grid): number => {
  const flash = createPointSet()

  for (let x = 0; x < width; x++) {
    for (let y = 0; y < height; y++) {
      grid[y][x]++
      if (grid[y][x] > 9) {
        flash.add([x, y])
      }
    }
  }

  const flashQueue = flash.values()
  while (flashQueue.length > 0) {
    const [fx, fy] = flashQueue.pop()!

    const xMin = Math.max(fx - 1, 0)
    const xMax = Math.min(fx + 1, width - 1)
    const yMin = Math.max(fy - 1, 0)
    const yMax = Math.min(fy + 1, height - 1)

    for (let x = xMin; x <= xMax; x++) {
      for (let y = yMin; y <= yMax; y++) {
        grid[y][x]++
        if (grid[y][x] > 9 && !flash.has([x, y])) {
          flash.add([x, y])
          flashQueue.push([x, y])
        }
      }
    }
  }

  for (const [x, y] of flash.values()) {
    grid[y][x] = 0
  }

  return flash.size()
}

export const solvePart1 = (input: string) => {
  const grid = parseInput(input)

  let flashes = 0
  for (let i = 0; i < 100; i++) {
    flashes += step(grid)
  }

  return flashes
}

export const solvePart2 = (input: string) => {
  const grid = parseInput(input)

  let i = 0
  while (++i) {
    if (step(grid) === 100) {
      return i
    }
  }
}
