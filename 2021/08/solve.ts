type Signal = string[]

type Entry = {
  uniqueSignals: Signal[]
  outputSignals: Signal[]
}

const parseSignals = (signals: string): Signal[] => {
  return signals.split(' ').map((signals) => signals.split(''))
}

const parseInput = (input: string): Entry[] => {
  return input
    .trim()
    .split('\n')
    .map((entry): Entry => {
      const [uniqueSignalsSection, outputSignalsSection] = entry.split(' | ')
      return {
        uniqueSignals: parseSignals(uniqueSignalsSection),
        outputSignals: parseSignals(outputSignalsSection),
      }
    })
}

const signalsEqual = (s1: Signal, s2: Signal): boolean => {
  return s1.length === s2.length && s2.every((o) => s1.includes(o))
}

const signalOverlap = (s1: Signal, s2: Signal): number => {
  return s1.filter((s) => s2.includes(s)).length
}

const evaluateEntry = ({ uniqueSignals, outputSignals }: Entry): number => {
  const signals: Signal[] = []

  const findSignal = (
    length: number,
    predicate: (signal: Signal) => boolean = () => true,
  ): Signal => {
    return uniqueSignals.find(
      (signal) => signal.length === length && predicate(signal),
    )!
  }

  signals[1] = findSignal(2)

  signals[4] = findSignal(4)

  signals[7] = findSignal(3)

  signals[8] = findSignal(7)

  signals[3] = findSignal(5, (s) => signalOverlap(s, signals[1]) === 2)

  signals[2] = findSignal(5, (s) => signalOverlap(s, signals[4]) === 2)

  signals[5] = findSignal(
    5,
    (s) => signalOverlap(s, signals[4]) === 3 && !signalsEqual(s, signals[3]),
  )

  signals[9] = findSignal(6, (s) => signalOverlap(s, signals[4]) === 4)

  signals[0] = findSignal(6, (s) => signalOverlap(s, signals[5]) === 4)

  signals[6] = findSignal(
    6,
    (s) => !(signalsEqual(s, signals[9]) || signalsEqual(s, signals[0])),
  )

  return Number(
    outputSignals
      .map((s) => signals.findIndex((o) => signalsEqual(o, s)))
      .join(''),
  )
}

export const solvePart1 = (input: string) => {
  return parseInput(input).reduce<number>(
    (acc, entry) =>
      acc +
      entry.outputSignals.filter((outputSignal) =>
        [2, 4, 3, 7].includes(outputSignal.length),
      ).length,
    0,
  )
}

export const solvePart2 = (input: string) => {
  return parseInput(input).reduce<number>(
    (acc, entry) => acc + evaluateEntry(entry),
    0,
  )
}
