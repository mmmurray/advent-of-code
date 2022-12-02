type Input = {
  initial: string
  rules: [string, string, string][]
  elements: string[]
}

const parseInput = (input: string): Input => {
  const [initial, rulesSection] = input.trim().split('\n\n')
  const uniqueElements = new Set<string>()
  const rules: Input['rules'] = rulesSection.split('\n').map((line) => {
    const [ab, c] = line.split(' -> ')
    const [a, b] = ab.split('')
    uniqueElements.add(c)
    return [a, b, c]
  })
  const elements = Array.from(uniqueElements.values()).sort()

  return { initial, rules, elements }
}

type PairIndex = number

type ElementIndex = number

type OptimizedInput = {
  pairCounts: number[]
  pairSubstituions: [PairIndex, PairIndex][]
  pairElements: [ElementIndex, ElementIndex][]
  elementsCount: number
}

const optimizeInput = (input: string): OptimizedInput => {
  const { initial, rules, elements } = parseInput(input)
  const elementsCount = elements.length
  const pairsCount = elementsCount * elementsCount

  const getPairIndex = (a: string, b: string): PairIndex =>
    elements.indexOf(a) * elementsCount + elements.indexOf(b)

  const pairCounts: number[] = Array(pairsCount).fill(0)
  const pairSubstituions: [PairIndex, PairIndex][] = Array(pairsCount).fill(0)
  const pairElements: [ElementIndex, ElementIndex][] = Array(pairsCount).fill(0)

  for (let i = 1; i < initial.length; i++) {
    const a = initial[i - 1]
    const b = initial[i]
    pairCounts[getPairIndex(a, b)]++
  }

  for (const [a, b, c] of rules) {
    const pairIndex = getPairIndex(a, b)
    pairSubstituions[pairIndex] = [getPairIndex(a, c), getPairIndex(c, b)]
    pairElements[pairIndex] = [elements.indexOf(a), elements.indexOf(b)]
  }

  return { pairCounts, pairSubstituions, pairElements, elementsCount }
}

const polymerize = (input: string, iterations: number): number => {
  let { pairCounts, pairSubstituions, pairElements, elementsCount } =
    optimizeInput(input)

  for (let iteration = 0; iteration < iterations; iteration++) {
    const next = Array(pairCounts.length).fill(0)
    for (let i = 0; i < pairCounts.length; i++) {
      const [pair0, pair1] = pairSubstituions[i]
      next[pair0] += pairCounts[i]
      next[pair1] += pairCounts[i]
    }
    pairCounts = next
  }

  const elementCounts = new Array(elementsCount).fill(0)
  for (let i = 0; i < pairCounts.length; i++) {
    const [e1, e2] = pairElements[i]
    elementCounts[e1] += pairCounts[i]
    elementCounts[e2] += pairCounts[i]
  }

  for (let i = 0; i < elementsCount; i++) {
    elementCounts[i] = Math.ceil(elementCounts[i] / 2)
  }

  const sorted = elementCounts.sort((a, b) => b - a)

  return sorted[0] - sorted[sorted.length - 1]
}

export const solvePart1 = (input: string) => {
  return polymerize(input, 10)
}

export const solvePart2 = (input: string) => {
  return polymerize(input, 40)
}
