type Bits = number[]

const parseInput = (input: string): Bits[] =>
  input
    .trim()
    .split('\n')
    .map((line) => line.split('').map(Number))

const bitsToDecimal = (bits: Bits): number => parseInt(bits.join(''), 2)

const sumNumbers = (numbers: Bits[]): Bits =>
  numbers.reduce((acc, bits) => acc.map((bit, index) => bit + bits[index]))

const booleanToBit = (b: boolean) => (b ? 1 : 0)

const calculateGamma = (numbers: Bits[]): Bits => {
  const counts = sumNumbers(numbers)
  const majority = numbers.length / 2

  return counts.map((bit) => booleanToBit(bit >= majority))
}

const calculateEpsilon = (numbers: Bits[]): Bits => {
  const counts = sumNumbers(numbers)
  const majority = numbers.length / 2

  return counts.map((bit) => booleanToBit(bit < majority))
}

const findNumber = (
  calculateCriteria: (numbers: Bits[]) => Bits,
  numbers: Bits[],
  position = 0,
): Bits => {
  if (numbers.length === 1) {
    return numbers[0]
  }

  const criteria = calculateCriteria(numbers)
  const nextNumbers = numbers.filter(
    (bits) => bits[position] === criteria[position],
  )

  return findNumber(calculateCriteria, nextNumbers, position + 1)
}

export const solvePart1 = (input: string) => {
  const numbers = parseInput(input)
  const gamma = bitsToDecimal(calculateGamma(numbers))
  const epsilon = bitsToDecimal(calculateEpsilon(numbers))

  return gamma * epsilon
}

export const solvePart2 = (input: string) => {
  const numbers = parseInput(input)
  const v1 = bitsToDecimal(findNumber(calculateGamma, numbers))
  const v2 = bitsToDecimal(findNumber(calculateEpsilon, numbers))

  return v1 * v2
}
