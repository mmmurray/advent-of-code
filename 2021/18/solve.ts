type SFNumber =
  | { $: 'Regular'; value: number }
  | { $: 'Pair'; l: SFNumber; r: SFNumber }

const parseSFNumber = (s: string): SFNumber => {
  type JSON = number | [JSON, JSON]
  const fromJSON = (json: JSON): SFNumber => {
    if (Array.isArray(json)) {
      return { $: 'Pair', l: fromJSON(json[0]), r: fromJSON(json[1]) }
    }
    return { $: 'Regular', value: json }
  }
  return fromJSON(JSON.parse(s))
}

const parseInput = (input: string): SFNumber[] => {
  return input.trim().split('\n').map(parseSFNumber)
}

const formatSFNumber = (n: SFNumber): string => {
  if (n.$ === 'Regular') {
    return String(n.value)
  }
  return `[${formatSFNumber(n.l)},${formatSFNumber(n.r)}]`
}

const increaseLeftmostRegular = (n: SFNumber, x: number): SFNumber => {
  if (n.$ === 'Regular') {
    return { $: 'Regular', value: n.value + x }
  }
  return {
    $: 'Pair',
    l: increaseLeftmostRegular(n.l, x),
    r: n.r,
  }
}

const increaseRightmostRegular = (n: SFNumber, x: number): SFNumber => {
  if (n.$ === 'Regular') {
    return { $: 'Regular', value: n.value + x }
  }
  return {
    $: 'Pair',
    l: n.l,
    r: increaseRightmostRegular(n.r, x),
  }
}

const explodeSFNumber = (n: SFNumber): SFNumber | null => {
  const rec = (
    n: SFNumber,
    depth: number,
  ): [SFNumber, [number, number]] | null => {
    if (n.$ === 'Pair' && depth === 4) {
      if (n.l.$ !== 'Regular' || n.r.$ !== 'Regular') {
        throw new Error(`Unexpected pair at depth 5`)
      }

      return [{ $: 'Regular', value: 0 }, [n.l.value, n.r.value]]
    }

    if (n.$ === 'Pair') {
      const explodeLeftResult = rec(n.l, depth + 1)
      if (explodeLeftResult) {
        const [l, [addL, addR]] = explodeLeftResult
        return [
          {
            $: 'Pair',
            l,
            r: increaseLeftmostRegular(n.r, addR),
          },
          [addL, 0],
        ]
      }

      const explodeRightResult = rec(n.r, depth + 1)
      if (explodeRightResult) {
        const [r, [addL, addR]] = explodeRightResult
        return [
          {
            $: 'Pair',
            l: increaseRightmostRegular(n.l, addL),
            r,
          },
          [0, addR],
        ]
      }
    }

    return null
  }

  const result = rec(n, 0)

  return result ? result[0] : null
}

const splitSFNumber = (n: SFNumber): SFNumber | null => {
  if (n.$ === 'Regular') {
    if (n.value < 10) {
      return null
    }

    return {
      $: 'Pair',
      l: { $: 'Regular', value: Math.floor(n.value / 2) },
      r: { $: 'Regular', value: Math.ceil(n.value / 2) },
    }
  }

  const splitLeftResult = splitSFNumber(n.l)
  if (splitLeftResult) {
    return {
      $: 'Pair',
      l: splitLeftResult,
      r: n.r,
    }
  }

  const splitRightResult = splitSFNumber(n.r)
  if (splitRightResult) {
    return {
      $: 'Pair',
      l: n.l,
      r: splitRightResult,
    }
  }

  return null
}

const reduceSFNumber = (n: SFNumber): SFNumber => {
  let current = n
  while (true) {
    const explodeResult = explodeSFNumber(current)
    if (explodeResult) {
      current = explodeResult
      continue
    }
    const splitResult = splitSFNumber(current)

    if (splitResult) {
      current = splitResult
      continue
    }
    break
  }
  return current
}

const addSFNumbers = (a: SFNumber, b: SFNumber): SFNumber => {
  return reduceSFNumber({ $: 'Pair', l: a, r: b })
}

const getSFNumberMagnitude = (n: SFNumber): number => {
  if (n.$ === 'Regular') {
    return n.value
  }

  return 3 * getSFNumberMagnitude(n.l) + 2 * getSFNumberMagnitude(n.r)
}

export const solvePart1 = (input: string) => {
  const numbers = parseInput(input)
  const sum = numbers.reduce((acc, n) => addSFNumbers(acc, n))

  return getSFNumberMagnitude(sum)
}

export const solvePart2 = (input: string) => {
  const numbers = parseInput(input)
  let maxMagnitude = 0

  for (let i = 1; i < numbers.length; i++) {
    for (let j = 0; j < i; j++) {
      const sum = addSFNumbers(numbers[i], numbers[j])
      const magnitude = getSFNumberMagnitude(sum)
      maxMagnitude = Math.max(maxMagnitude, magnitude)
    }
  }

  return maxMagnitude
}
