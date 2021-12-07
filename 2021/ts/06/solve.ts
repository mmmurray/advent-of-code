type Timers = {
  get: (value: number) => number
  set: (value: number, count: number) => void
  count: () => number
}

const createTimers = (): Timers => {
  const timers = new Map<number, number>()

  for (let i = 0; i < 9; i++) {
    timers.set(i, 0)
  }

  return {
    get: (value) => timers.get(value)!,
    set: (value, count) => {
      timers.set(value, count)
    },
    count: () =>
      Array.from(timers.values()).reduce((acc, count) => acc + count),
  }
}

const parseInput = (input: string): Timers => {
  const timers = createTimers()

  input
    .trim()
    .split(',')
    .map(Number)
    .forEach((timer) => {
      timers.set(timer, timers.get(timer) + 1)
    })

  return timers
}

const evolve = (timers: Timers): Timers => {
  const newTimers = createTimers()
  const zeroCount = timers.get(0)

  for (let i = 1; i < 9; i++) {
    newTimers.set(i - 1, timers.get(i))
  }

  newTimers.set(6, newTimers.get(6) + zeroCount)
  newTimers.set(8, zeroCount)

  return newTimers
}

const evolveTimes = (timers: Timers, n: number): Timers => {
  if (n === 0) {
    return timers
  }

  return evolveTimes(evolve(timers), n - 1)
}

export const solvePart1 = (input: string) => {
  return evolveTimes(parseInput(input), 80).count()
}

export const solvePart2 = (input: string) => {
  return evolveTimes(parseInput(input), 256).count()
}
