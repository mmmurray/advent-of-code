import { range } from 'core/common'
import { vec2, Vec2 } from 'core/math'

type Region = {
  minX: number
  minY: number
  maxX: number
  maxY: number
}

type Vec2Set = {
  add: (v: Vec2) => void
  has: (v: Vec2) => boolean
  setDefaultOutsideOfRegion: (region: Region) => void
  values: () => Vec2[]
}

const createVec2Set = (): Vec2Set => {
  const set = new Map<string, Vec2>()
  let defaultOutOfRegion: Region | null = null
  return {
    add: (v) => {
      set.set(v.join(','), v)
    },
    has: (v) => {
      const [x, y] = v
      const r = defaultOutOfRegion
      if (r && (x < r.minX || x > r.maxX || y < r.minY || y > r.maxY)) {
        return true
      }

      return set.has(v.join(','))
    },
    setDefaultOutsideOfRegion: (r) => {
      defaultOutOfRegion = r
    },
    values: () => {
      return Array.from(set.values())
    },
  }
}

type Input = {
  algorithm: boolean[]
  image: Vec2Set
}

const parseInput = (input: string): Input => {
  const [a, b] = input.trim().split('\n\n')
  const algorithm = a.split('').map((c) => c === '#')
  const image = createVec2Set()
  b.split('\n').forEach((line, y) => {
    line.split('').forEach((c, x) => {
      if (c === '#') {
        image.add(vec2(x, y))
      }
    })
  })
  return { algorithm, image }
}

const generateOutputImage = (
  algorithm: boolean[],
  image: Vec2Set,
  flipFlop: boolean,
): Vec2Set => {
  const output = createVec2Set()

  let minX = Infinity
  let maxX = -Infinity
  let minY = Infinity
  let maxY = -Infinity

  image.values().forEach(([x, y]) => {
    minX = Math.min(minX, x)
    maxX = Math.max(maxX, x)
    minY = Math.min(minY, y)
    maxY = Math.max(maxY, y)
  })

  const getBit = ([x, y]: Vec2): number => {
    return image.has(vec2(x, y)) ? 1 : 0
  }

  for (let x = minX - 1; x <= maxX + 1; x++) {
    for (let y = minY - 1; y <= maxY + 1; y++) {
      const bits = [
        getBit(vec2(x + 1, y + 1)),
        getBit(vec2(x, y + 1)),
        getBit(vec2(x - 1, y + 1)),
        getBit(vec2(x + 1, y)),
        getBit(vec2(x, y)),
        getBit(vec2(x - 1, y)),
        getBit(vec2(x + 1, y - 1)),
        getBit(vec2(x, y - 1)),
        getBit(vec2(x - 1, y - 1)),
      ]

      const index = bits.reduce((acc, b, i) => acc + (b << i), 0)

      if (algorithm[index]) {
        output.add(vec2(x, y))
      }
    }
  }

  if (flipFlop) {
    output.setDefaultOutsideOfRegion({
      minX: minX - 1,
      minY: minY - 1,
      maxX: maxX + 1,
      maxY: maxY + 1,
    })
  }

  return output
}

const iterateImage = (input: string, iterations: number): number => {
  const { algorithm, image } = parseInput(input)
  const flipFlop = algorithm[0]

  const output = range(0, iterations).reduce(
    (acc, x) => generateOutputImage(algorithm, acc, flipFlop && x % 2 === 0),
    image,
  )

  return output.values().length
}

export const solvePart1 = (input: string) => {
  return iterateImage(input, 2)
}

export const solvePart2 = (input: string) => {
  return iterateImage(input, 50)
}
