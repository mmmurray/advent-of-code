import { findShortestPathWeight } from 'core/algorithms'
import { array2, range } from 'core/common'

type Vec2 = [number, number]

type Grid = {
  width: number
  height: number
  cells: number[][]
}

const parseInput = (input: string): Grid => {
  const lines = input.trim().split('\n')
  return {
    width: lines[0].length,
    height: lines.length,
    cells: lines.map((line) => line.split('').map(Number)),
  }
}

const vec2ManhattenDistance = ([x1, y1]: Vec2, [x2, y2]: Vec2): number => {
  return Math.abs(x2 - x1) + Math.abs(y2 - y1)
}

const findLeastRiskyPath = ({ width, height, cells }: Grid) => {
  const start: Vec2 = [0, 0]
  const target: Vec2 = [width - 1, height - 1]

  const nodesById: Vec2[] = range(0, width * height).map((id): Vec2 => {
    const y = Math.floor(id / width)
    const x = id - y * width
    return [x, y]
  })

  const getNodeId = ([x, y]: Vec2): number => {
    return y * width + x
  }

  return findShortestPathWeight({
    startNode: getNodeId(start),
    targetNode: getNodeId(target),
    calculateDistance: (_, id) => {
      const [x, y] = nodesById[id]
      return cells[y][x]
    },
    calculateHeuristic: (id) => {
      const node = nodesById[id]
      return vec2ManhattenDistance(node, target)
    },
    getAdjacentNodes: (id) => {
      const [x, y] = nodesById[id]
      const neighbors: number[] = []
      if (x > 0) {
        neighbors.push(getNodeId([x - 1, y]))
      }
      if (x < width - 1) {
        neighbors.push(getNodeId([x + 1, y]))
      }
      if (y > 0) {
        neighbors.push(getNodeId([x, y - 1]))
      }
      if (y < height - 1) {
        neighbors.push(getNodeId([x, y + 1]))
      }
      return neighbors
    },
  })
}

const expandGrid = (grid: Grid, multiplier: number): Grid => {
  const { width, height, cells } = grid
  const expandedWidth = width * multiplier
  const expandedHeight = height * multiplier
  const expandedCells: number[][] = array2(expandedHeight, expandedWidth, 0)

  for (let y = 0; y < expandedHeight; y++) {
    for (let x = 0; x < expandedWidth; x++) {
      const original = cells[y % height][x % width]
      const dx = Math.floor(x / width)
      const dy = Math.floor(y / height)
      const v = original + dx + dy
      expandedCells[y][x] = ((v - 1) % 9) + 1
    }
  }

  return {
    width: expandedWidth,
    height: expandedHeight,
    cells: expandedCells,
  }
}

export const solvePart1 = (input: string) => {
  const grid = parseInput(input)

  return findLeastRiskyPath(grid)
}

export const solvePart2 = (input: string) => {
  const grid = parseInput(input)
  const expandedGrid = expandGrid(grid, 5)

  return findLeastRiskyPath(expandedGrid)
}
