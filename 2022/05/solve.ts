import { chunksOf, range } from '@common'

type Stack = string[]

type Step = {
  quantity: number
  from: number
  to: number
}

const parseStep = (input: string): Step => {
  const matches = /^move (\d+) from (\d+) to (\d+)$/.exec(input)!

  return {
    quantity: Number(matches[1]),
    from: Number(matches[2]) - 1,
    to: Number(matches[3]) - 1,
  }
}

const parseStackLayer = (input: string): (string | null)[] => {
  const chars = input.split('')
  const charGroups = chunksOf(4, chars)

  return charGroups.map((charGroup) => {
    const char = charGroup[1]
    return char === ' ' ? null : char
  })
}

const parseInput = (input: string): [Stack[], Step[]] => {
  const [stacksInput, stepsInput] = input.trimEnd().split('\n\n')
  const layers = stacksInput.split('\n').slice(0, -1).map(parseStackLayer)

  const stacks: Stack[] = range(0, layers[0].length).map((stackIndex) => {
    return layers.flatMap((layer) => {
      const item = layer[stackIndex]
      return item === null ? [] : [item]
    })
  })

  const steps: Step[] = stepsInput.split('\n').map(parseStep)

  return [stacks, steps]
}

const executeStep = (
  stacks: Stack[],
  step: Step,
  moveOneAtATime: boolean,
): Stack[] => {
  const newStacks = [...stacks]
  const fromStack = [...stacks[step.from]]
  const toStack = [...stacks[step.to]]
  const items = fromStack.splice(0, step.quantity)

  toStack.unshift(...(moveOneAtATime ? items.reverse() : items))
  newStacks[step.from] = fromStack
  newStacks[step.to] = toStack

  return newStacks
}

const run = (input: string, moveOneAtATime: boolean): string => {
  const [stacks, steps] = parseInput(input)

  const finalStacks = steps.reduce(
    (acc, step) => executeStep(acc, step, moveOneAtATime),
    stacks,
  )

  return finalStacks.map((stack) => stack[0]).join('')
}

export const solvePart1 = (input: string): string => {
  return run(input, true)
}

export const solvePart2 = (input: string): string => {
  return run(input, false)
}
