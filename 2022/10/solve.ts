type Instruction =
  | {
      $: 'noop'
    }
  | {
      $: 'addx'
      value: number
    }

const parseInput = (input: string): Instruction[] => {
  return input
    .trim()
    .split('\n')
    .map<Instruction>((line) => {
      if (line === 'noop') {
        return { $: 'noop' }
      }
      const [, value] = line.split(' ')
      return { $: 'addx', value: Number(value) }
    })
}

const getSpriteCyclePositions = (instructions: Instruction[]): number[] => {
  const positions: number[] = []
  let x = 1

  for (const instruction of instructions) {
    switch (instruction.$) {
      case 'noop': {
        positions.push(x)
        break
      }
      case 'addx': {
        positions.push(x)
        positions.push(x)
        x += instruction.value
        break
      }
    }
  }

  return positions
}

export const solvePart1 = (input: string): number => {
  const instructions = parseInput(input)
  const positions = getSpriteCyclePositions(instructions)
  const samples = [20, 60, 100, 140, 180, 220]

  return samples.reduce((acc, sample) => {
    return acc + sample * positions[sample - 1]
  }, 0)
}

export const solvePart2 = (input: string): string => {
  const instructions = parseInput(input)
  const positions = getSpriteCyclePositions(instructions)
  const output: string[] = []
  const screenWidth = 40
  const screenHeight = 6

  for (let y = 0; y < screenHeight; y++) {
    const row: string[] = []

    for (let x = 0; x < screenWidth; x++) {
      const sprite = positions[y * screenWidth + x]
      const lit = x === sprite - 1 || x === sprite || x === sprite + 1

      row.push(lit ? '#' : ' ')
    }

    output.push(row.join(''))
  }

  return output.join('\n')
}
