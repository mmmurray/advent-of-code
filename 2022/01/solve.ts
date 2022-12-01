import { splitWhen, sum } from '@common'

const getTotalCaloriesByElf = (input: string): number[] => {
  const lines = input.trim().split('\n')
  const linesByElf = splitWhen((line) => line.length === 0, lines)
  const caloriesByElf = linesByElf.map((lines) => lines.map(Number))
  const totalCaloriesByElf = caloriesByElf.map(sum)

  return totalCaloriesByElf
}

export const solvePart1 = (input: string): number => {
  const totalCaloriesByElf = getTotalCaloriesByElf(input)

  return Math.max(...totalCaloriesByElf)
}

export const solvePart2 = (input: string): number => {
  const totalCaloriesByElf = getTotalCaloriesByElf(input)
  const sortedTotalCalories = [...totalCaloriesByElf].sort((a, b) => b - a)

  return sum(sortedTotalCalories.slice(0, 3))
}
