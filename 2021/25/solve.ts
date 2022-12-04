type Cell = '.' | '>' | 'v'

type Grid = {
  width: number
  height: number
  cells: Cell[]
}

const parseInput = (input: string): Grid => {
  const lines = input.trim().split('\n')
  const height = lines.length
  const cells = lines.flatMap((line) => line.split('') as Cell[])
  const width = cells.length / height
  return { width, height, cells }
}

const stepCellType = (
  grid: Grid,
  cellType: Cell,
  move: [number, number],
): [Grid, boolean] => {
  const newCells = [...grid.cells]
  let moved = false

  grid.cells.forEach((cell, i1) => {
    const y1 = Math.floor(i1 / grid.width)
    const x1 = i1 - y1 * grid.width

    if (cell === cellType) {
      const y2 = (y1 + move[1]) % grid.height
      const x2 = (x1 + move[0]) % grid.width
      const i2 = y2 * grid.width + x2

      if (grid.cells[i2] === '.') {
        newCells[i1] = '.'
        newCells[i2] = cellType
        moved = true
      }
    }
  })

  return [{ ...grid, cells: newCells }, moved]
}

const step = (grid0: Grid): [Grid, boolean] => {
  const [grid1, moved1] = stepCellType(grid0, '>', [1, 0])
  const [grid2, moved2] = stepCellType(grid1, 'v', [0, 1])

  return [grid2, moved1 || moved2]
}

const simulate = (grid: Grid): number => {
  let moves = 0
  let currentGrid = grid

  while (true) {
    const [nextGrid, moved] = step(currentGrid)
    currentGrid = nextGrid
    moves++

    if (!moved) {
      break
    }
  }

  return moves
}

export const solvePart1 = (input: string): number => {
  return simulate(parseInput(input))
}
