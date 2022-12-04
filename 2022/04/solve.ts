type Range = [number, number]

const parseRange = (input: string): Range => {
  const [a, b] = input.split('-')
  return [Number(a), Number(b)]
}

const parseInput = (input: string): [Range, Range][] => {
  return input
    .trim()
    .split('\n')
    .map((line) => {
      const [rangeA, rangeB] = line.split(',')
      return [parseRange(rangeA), parseRange(rangeB)]
    })
}

const rangeContains = (rOuter: Range, rInner: Range): boolean => {
  return rOuter[0] >= rInner[0] && rOuter[1] <= rInner[1]
}

const rangesOverlay = (rA: Range, rB: Range): boolean => {
  return rA[1] >= rB[0] && rA[0] <= rB[1]
}

export const solvePart1 = (input: string): number => {
  const rangePairs = parseInput(input)

  const fullyContainedRangePairs = rangePairs.filter(([rA, rB]) => {
    return rangeContains(rA, rB) || rangeContains(rB, rA)
  })

  return fullyContainedRangePairs.length
}

export const solvePart2 = (input: string): number => {
  const rangePairs = parseInput(input)

  const overlappingRangePairs = rangePairs.filter(([rA, rB]) => {
    return rangesOverlay(rA, rB)
  })

  return overlappingRangePairs.length
}
