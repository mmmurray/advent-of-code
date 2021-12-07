const parseInput = (input: string): number[] => {
  return input.trim().split(',').map(Number)
}

const optimizeFuelCostForAlignment = (
  positions: number[],
  fuelCostForMovement: (movement: number) => number,
): number => {
  const minPosition = Math.min(...positions)
  const maxPosition = Math.max(...positions)
  let optimalFuelCost = Infinity

  for (let target = minPosition; target <= maxPosition; target++) {
    const fuelCost = positions.reduce<number>(
      (acc, position) => acc + fuelCostForMovement(Math.abs(position - target)),
      0,
    )

    if (fuelCost < optimalFuelCost) {
      optimalFuelCost = fuelCost
    }
  }

  return optimalFuelCost
}

export const solvePart1 = (input: string) => {
  return optimizeFuelCostForAlignment(parseInput(input), (movement) => movement)
}

export const solvePart2 = (input: string) => {
  return optimizeFuelCostForAlignment(
    parseInput(input),
    (movement) => (movement * (movement + 1)) / 2,
  )
}
