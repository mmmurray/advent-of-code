const parseInput = (input: string): number[] =>
  input.trim().split('\n').map(Number)

const sumWindow = (
  depths: number[],
  window: number,
  startIndex: number,
): number => {
  const rec = (depths: number[], window: number, sum: number): number => {
    if (window === 0) {
      return sum
    }

    return rec(depths.slice(1), window - 1, sum + depths[0])
  }

  return rec(depths.slice(startIndex), window, 0)
}

const calculateIncreases = (depths: number[], window: number): number => {
  const rec = (depths: number[], increases: number): number => {
    if (depths.length === 1) {
      return increases
    }

    const increase = sumWindow(depths, window, 1) > sumWindow(depths, window, 0)

    return rec(depths.slice(1), increases + (increase ? 1 : 0))
  }

  return rec(depths, 0)
}

export const solvePart1 = (input: string) => {
  const depths = parseInput(input)
  const window = 1

  return calculateIncreases(depths, window)
}

export const solvePart2 = (input: string) => {
  const depths = parseInput(input)
  const window = 3

  return calculateIncreases(depths, window)
}
