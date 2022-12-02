import { sum } from '@common'
import { mod } from '@math'

const strategyMapping: { [symbol: string]: number } = {
  A: 0,
  B: 1,
  C: 2,
  X: 0,
  Y: 1,
  Z: 2,
}

const parseInput = (input: string): [number, number][] => {
  return input
    .trim()
    .split('\n')
    .map((line) => {
      const [p1, p2] = line.split(' ')

      return [strategyMapping[p1], strategyMapping[p2]]
    })
}

const mod3 = (x: number): number => mod(x, 3)

const getRoundScore = ([p1Pick, p2Pick]: [number, number]): number => {
  const choiceScore = p2Pick + 1
  const resultScore = mod3(mod3(p2Pick - p1Pick) + 1) * 3

  return resultScore + choiceScore
}

const play = (
  input: string,
  strategy: (x: [number, number]) => number,
): number => {
  const rounds = parseInput(input)
  const scores = rounds.map((round) => {
    return getRoundScore([round[0], strategy(round)])
  })

  return sum(scores)
}

export const solvePart1 = (input: string): number => {
  return play(input, ([, p2Pick]) => p2Pick)
}

export const solvePart2 = (input: string): number => {
  return play(input, ([p1Pick, p2Strat]) => {
    const outcomeOffset = mod3(p2Strat - 1)

    return mod3(p1Pick + outcomeOffset)
  })
}
