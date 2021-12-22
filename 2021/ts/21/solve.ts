import { array5 } from 'core/common'

const parseInput = (input: string): [number, number] => {
  const pattern =
    /^Player 1 starting position: (\d+)\nPlayer 2 starting position: (\d+)$/
  const matches = pattern.exec(input.trim())!
  return [Number(matches[1]), Number(matches[2])]
}

const playGame1 = (input: string): number => {
  const [p1, p2] = parseInput(input)

  const positions: number[] = [p1 - 1, p2 - 1]
  const scores: number[] = [0, 0]

  let dieState = 0
  const roll = (): number => {
    const value = (dieState % 100) + 1
    dieState++
    return value
  }

  const takeTurn = (playerIndex: number): boolean => {
    const rollResult = [roll(), roll(), roll()].reduce((acc, r) => acc + r)
    const position = (positions[playerIndex] + rollResult) % 10
    const score = scores[playerIndex] + position + 1
    positions[playerIndex] = position
    scores[playerIndex] = score
    return score >= 1000
  }

  while (true) {
    const p1Won = takeTurn(0)
    if (p1Won) {
      return dieState * scores[1]
    }

    const p2Won = takeTurn(1)
    if (p2Won) {
      return dieState * scores[0]
    }
  }
}

const playGame2 = (input: string): number => {
  const [p1, p2] = parseInput(input)
  const scoreToWin = 21
  const cache = array5<[number, number] | null>(
    2, // player index
    10, // player 1 position
    10, // player 2 position
    scoreToWin + 1, // player 1 score
    scoreToWin + 1, // player 2 score
    null,
  )

  const rec = (
    playerIndex: number,
    positions: number[],
    scores: number[],
  ): [number, number] => {
    const cached =
      cache[playerIndex][positions[0]][positions[1]][scores[0]][scores[1]]

    if (cached) {
      return cached
    }

    let sum: [number, number] = [0, 0]
    for (let d1 = 1; d1 <= 3; d1++) {
      for (let d2 = 1; d2 <= 3; d2++) {
        for (let d3 = 1; d3 <= 3; d3++) {
          const rollResult = d1 + d2 + d3
          const position = (positions[playerIndex] + rollResult) % 10
          const score = scores[playerIndex] - (position + 1)

          let result: [number, number]
          if (score <= 0) {
            result = [0, 0]
            result[playerIndex] = 1
          } else {
            const nextPlayerIndex = (playerIndex + 1) % 2
            const nextPositions = [positions[0], positions[1]]
            const nextScores = [scores[0], scores[1]]
            nextPositions[playerIndex] = position
            nextScores[playerIndex] = score
            result = rec(nextPlayerIndex, nextPositions, nextScores)
          }

          sum = [sum[0] + result[0], sum[1] + result[1]]
        }
      }
    }

    cache[playerIndex][positions[0]][positions[1]][scores[0]][scores[1]] = sum

    return sum
  }

  const [w1, w2] = rec(0, [p1 - 1, p2 - 1], [scoreToWin, scoreToWin])

  return Math.max(w1, w2)
}

export const solvePart1 = (input: string) => {
  return playGame1(input)
}

export const solvePart2 = (input: string) => {
  return playGame2(input)
}
