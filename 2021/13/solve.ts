type Vec2 = [number, number]

type Fold = { $: 'x'; v: number } | { $: 'y'; v: number }

type Input = {
  points: Vec2[]
  folds: Fold[]
}

type Grid = {
  cells: number[][]
  width: number
  height: number
}

const parseInput = (input: string): Input => {
  const [pointsSection, foldsSection] = input.trim().split('\n\n')
  const points: Vec2[] = pointsSection.split('\n').map((line) => {
    const [x, y] = line.split(',').map(Number)
    return [x, y]
  })
  const folds: Fold[] = foldsSection.split('\n').map((line) => {
    const [, axis, v] = /^fold along ([xy])=(\d+)$/.exec(line)!
    return { $: axis as Fold['$'], v: Number(v) }
  })
  return { points, folds }
}

const createGrid = (points: Vec2[]): Grid => {
  const width = Math.max(...points.map(([x]) => x)) + 1
  const height = Math.max(...points.map(([, y]) => y)) + 1

  const cells = Array(height)
    .fill(0)
    .map(() => Array<number>(width).fill(0))

  points.forEach(([x, y]) => {
    cells[y][x] = 1
  })

  return { cells, width, height }
}

const applyFold = ({ cells, width, height }: Grid, fold: Fold): Grid => {
  switch (fold.$) {
    case 'x': {
      for (let y = 0; y < height; y++) {
        for (let x = fold.v + 1; x < width; x++) {
          cells[y][fold.v * 2 - x] += cells[y][x]
        }
      }

      return { cells, width: fold.v, height }
    }
    case 'y': {
      for (let y = fold.v + 1; y < height; y++) {
        for (let x = 0; x < width; x++) {
          cells[fold.v * 2 - y][x] += cells[y][x]
        }
      }

      return { cells, width, height: fold.v }
    }
  }
}

const applyFolds = (grid: Grid, folds: Fold[]): Grid => {
  return folds.reduce((acc, fold) => applyFold(acc, fold), grid)
}

const countVisibleDots = ({ cells, width, height }: Grid): number => {
  let total = 0
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      total += Math.min(1, cells[y][x])
    }
  }
  return total
}

const printGrid = ({ cells, width, height }: Grid): string => {
  const output: string[] = []
  for (let y = 0; y < height; y++) {
    let line = ''
    for (let x = 0; x < width; x++) {
      line += cells[y][x] > 0 ? '#' : '.'
    }
    output.push(line)
  }
  return output.join('\n')
}

export const solvePart1 = (input: string) => {
  const { points, folds } = parseInput(input)
  const grid = createGrid(points)

  return countVisibleDots(applyFold(grid, folds[0]))
}

export const solvePart2 = (input: string) => {
  const { points, folds } = parseInput(input)
  const grid = createGrid(points)

  return printGrid(applyFolds(grid, folds))
}
