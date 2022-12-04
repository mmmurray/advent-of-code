import { splitWhen } from '@common'

type Registers = { w: number; x: number; y: number; z: number }

type Register = keyof Registers

type Value = Register | number

type Instruction =
  | { $: 'inp'; a: Register }
  | { $: 'add'; a: Register; b: Value }
  | { $: 'mul'; a: Register; b: Value }
  | { $: 'div'; a: Register; b: Value }
  | { $: 'mod'; a: Register; b: Value }
  | { $: 'eql'; a: Register; b: Value }

type Machine = {
  inputs: number[]
  registers: Registers
}

export const parseInput = (input: string): Instruction[] => {
  return input
    .trim()
    .split('\n')
    .map((line): Instruction => {
      const [$, a, bString] = line.split(' ')
      const bNum = Number(bString)
      const b = Number.isNaN(bNum) ? bString : bNum
      return { $, a, b } as Instruction
    })
}

const createMachine = (inputs: number[]): Machine => {
  return {
    inputs,
    registers: { w: 0, x: 0, y: 0, z: 0 },
  }
}

const step = (machine: Machine, instruction: Instruction): Machine => {
  const valueOf = (r: Register | number): number => {
    return typeof r === 'number' ? r : machine.registers[r]
  }

  switch (instruction.$) {
    case 'inp': {
      const [input, ...inputs] = machine.inputs
      const registers = { ...machine.registers, [instruction.a]: input }
      return { inputs, registers }
    }
    case 'add': {
      const result = valueOf(instruction.a) + valueOf(instruction.b)
      const registers = { ...machine.registers, [instruction.a]: result }
      return { inputs: machine.inputs, registers }
    }
    case 'mul': {
      const result = valueOf(instruction.a) * valueOf(instruction.b)
      const registers = { ...machine.registers, [instruction.a]: result }
      return { inputs: machine.inputs, registers }
    }
    case 'div': {
      const result = Math.floor(valueOf(instruction.a) / valueOf(instruction.b))
      const registers = { ...machine.registers, [instruction.a]: result }
      return { inputs: machine.inputs, registers }
    }
    case 'mod': {
      const result = valueOf(instruction.a) % valueOf(instruction.b)
      const registers = { ...machine.registers, [instruction.a]: result }
      return { inputs: machine.inputs, registers }
    }
    case 'eql': {
      const result = valueOf(instruction.a) === valueOf(instruction.b) ? 1 : 0
      const registers = { ...machine.registers, [instruction.a]: result }
      return { inputs: machine.inputs, registers }
    }
  }
}

export const execute = (
  instructions: Instruction[],
  inputs: number[],
): Registers => {
  return instructions.reduce(step, createMachine(inputs)).registers
}

const solve = (instructions: Instruction[], searchDirection: 1 | -1) => {
  const blocks = splitWhen((i) => i.$ === 'inp', instructions)
    .slice(1)
    .map((instructions): Instruction[] => [
      { $: 'inp', a: 'w' },
      ...instructions,
    ])

  const cache: { [key in string]?: number[] | null } = {}

  const wFirst = searchDirection === 1 ? 1 : 9
  const wLast = searchDirection === 1 ? 10 : 0
  const wStep = searchDirection

  const rec = (blockIndex: number, z: number): number[] | null => {
    const key = JSON.stringify([blockIndex, z])
    const cached = cache[key]
    if (cached !== undefined) {
      return cached
    }

    let result: number[] | null = null

    if (blockIndex > 13) {
      result = z === 0 ? [] : null
    } else {
      for (let w = wFirst; w !== wLast; w += wStep) {
        const blockInstructions: Instruction[] = blocks[blockIndex]
        const blockMachine: Machine = {
          inputs: [w],
          registers: { w: 0, x: 0, y: 0, z },
        }
        const { registers } = blockInstructions.reduce(step, blockMachine)
        const nextAcc = rec(blockIndex + 1, registers.z)

        if (nextAcc) {
          result = [w, ...nextAcc]
          break
        }
      }
    }

    cache[key] = result
    return result
  }

  return rec(0, 0)!.join('')
}

export const solvePart1 = (input: string): string => {
  return solve(parseInput(input), -1)
}

export const solvePart2 = (input: string): string => {
  return solve(parseInput(input), 1)
}
