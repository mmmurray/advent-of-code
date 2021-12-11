const parseInput = (input: string): string[] => {
  return input.trim().split('\n')
}

const bracketMap: { [c: string]: string } = {
  '(': ')',
  '[': ']',
  '{': '}',
  '<': '>',
}

const bracketErrorScores: { [c: string]: number } = {
  ')': 3,
  ']': 57,
  '}': 1197,
  '>': 25137,
}

const bracketCompletionScores: { [c: string]: number } = {
  ')': 1,
  ']': 2,
  '}': 3,
  '>': 4,
}

type ParseResult =
  | { $: 'Error'; invalid: string }
  | { $: 'Incomplete'; completions: string[] }

const parseLine = (line: string): ParseResult => {
  const stack: string[] = []

  for (const c of line.split('')) {
    if (bracketMap[c]) {
      stack.push(c)
    } else if (c !== bracketMap[stack.pop()!]) {
      return { $: 'Error', invalid: c }
    }
  }

  return {
    $: 'Incomplete',
    completions: stack.reverse().map((c) => bracketMap[c]),
  }
}

const completionScore = (completions: string[]): number => {
  return completions.reduce((acc, c) => acc * 5 + bracketCompletionScores[c], 0)
}

export const solvePart1 = (input: string) => {
  const lines = parseInput(input)

  const score = lines.reduce((acc, line) => {
    const result = parseLine(line)
    return acc + (result.$ === 'Error' ? bracketErrorScores[result.invalid] : 0)
  }, 0)

  return score
}

export const solvePart2 = (input: string) => {
  const lines = parseInput(input)

  const scores = lines.reduce<number[]>((acc, line) => {
    const result = parseLine(line)
    if (result.$ === 'Incomplete') {
      acc.push(completionScore(result.completions))
    }
    return acc
  }, [])

  return scores.sort((a, b) => a - b)[Math.floor(scores.length / 2)]
}
