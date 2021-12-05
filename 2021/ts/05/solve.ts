type Vec2 = [number, number]

type Line = [Vec2, Vec2]

const vec2Add = ([x1, y1]: Vec2, [x2, y2]: Vec2): Vec2 => [x1 + x2, y1 + y2]

const vec2Equal = ([x1, y1]: Vec2, [x2, y2]: Vec2): boolean =>
  x1 === x2 && y1 === y2

const parsePoint = (point: string): Vec2 => {
  const [x, y] = point.split(',').map(Number)
  return [x, y]
}

const parseInput = (input: string): Line[] => {
  return input
    .trim()
    .split('\n')
    .map((line) => {
      const [p1, p2] = line.split(' -> ')
      return [parsePoint(p1), parsePoint(p2)]
    })
}

const isLineAxisAligned = (line: Line): boolean => {
  const [[x1, y1], [x2, y2]] = line
  return x1 === x2 || y1 === y2
}

type PointCounter = {
  inc: (point: Vec2) => void
  overlapping: () => number
}

const createPointCounter = (): PointCounter => {
  const points = new Map<string, number>()
  const overlapping = new Set<string>()

  return {
    inc: ([x, y]) => {
      const key = `${x},${y}`
      const current = points.get(key) || 0
      if (current === 1) {
        overlapping.add(key)
      }
      points.set(key, current + 1)
    },
    overlapping: () => overlapping.size,
  }
}

const getLineStep = (line: Line): Vec2 => {
  const [[x1, y1], [x2, y2]] = line

  return [Math.sign(x2 - x1), Math.sign(y2 - y1)]
}

const getPointsOnLine = (line: Line): Vec2[] => {
  const step = getLineStep(line)
  let current = line[0]
  const points: Vec2[] = [current]

  while (!vec2Equal(current, line[1])) {
    current = vec2Add(current, step)
    points.push(current)
  }

  return points
}

export const solvePart1 = (input: string) => {
  const lines = parseInput(input)
  const pointCounter = createPointCounter()

  lines.filter(isLineAxisAligned).forEach((line) => {
    getPointsOnLine(line).forEach(pointCounter.inc)
  })

  return pointCounter.overlapping()
}

export const solvePart2 = (input: string) => {
  const lines = parseInput(input)
  const pointCounter = createPointCounter()

  lines.forEach((line) => {
    getPointsOnLine(line).forEach(pointCounter.inc)
  })

  return pointCounter.overlapping()
}
