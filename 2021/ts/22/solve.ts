type Cuboid = {
  x1: number
  x2: number
  y1: number
  y2: number
  z1: number
  z2: number
}

const cuboid = (x1 = 0, x2 = 0, y1 = 0, y2 = 0, z1 = 0, z2 = 0): Cuboid => {
  return { x1, x2, y1, y2, z1, z2 }
}

const cuboidEnclosure = (cuboids: Cuboid[]): Cuboid => {
  const e = cuboid()
  for (const c of cuboids) {
    e.x1 = Math.min(e.x1, c.x1)
    e.x2 = Math.max(e.x2, c.x2)
    e.y1 = Math.min(e.y1, c.y1)
    e.y2 = Math.max(e.y2, c.y2)
    e.z1 = Math.min(e.z1, c.z1)
    e.z2 = Math.max(e.z2, c.z2)
  }
  return e
}

const cuboidIsValid = (c: Cuboid): boolean => {
  return c.x1 < c.x2 && c.y1 < c.y2 && c.z1 < c.z2
}

const cuboidIntersection = (c1: Cuboid, c2: Cuboid): Cuboid | null => {
  const intersection = cuboid(
    Math.max(c1.x1, c2.x1),
    Math.min(c1.x2, c2.x2),
    Math.max(c1.y1, c2.y1),
    Math.min(c1.y2, c2.y2),
    Math.max(c1.z1, c2.z1),
    Math.min(c1.z2, c2.z2),
  )

  return cuboidIsValid(intersection) ? intersection : null
}

const cuboidVolume = (c: Cuboid): number => {
  return (c.x2 - c.x1) * (c.y2 - c.y1) * (c.z2 - c.z1)
}

type Step = {
  on: boolean
  region: Cuboid
}

const parseInput = (input: string): Step[] => {
  return input
    .trim()
    .split('\n')
    .map((line) => {
      const p =
        /^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$/
      const matches = p.exec(line)!

      return {
        on: matches[1] === 'on',
        region: cuboid(
          Number(matches[2]),
          Number(matches[3]) + 1,
          Number(matches[4]),
          Number(matches[5]) + 1,
          Number(matches[6]),
          Number(matches[7]) + 1,
        ),
      }
    })
}

const cubesOnInRegionAfterStep = (
  steps: Step[],
  region: Cuboid,
  stepIndex: number,
): number => {
  const step = steps[stepIndex]
  const intersect = cuboidIntersection(region, step.region)

  if (stepIndex === 0) {
    return step.on && intersect ? cuboidVolume(intersect) : 0
  }

  const prevRegion = cubesOnInRegionAfterStep(steps, region, stepIndex - 1)
  if (!intersect) {
    return prevRegion
  }

  const prevIntersect = cubesOnInRegionAfterStep(
    steps,
    intersect,
    stepIndex - 1,
  )

  if (step.on) {
    return prevRegion - prevIntersect + cuboidVolume(intersect)
  }

  return prevRegion - prevIntersect
}

export const solvePart1 = (input: string) => {
  const initRegion = cuboid(-50, 51, -50, 51, -50, 51)
  const steps = parseInput(input).reduce<Step[]>((acc, step) => {
    const intersect = cuboidIntersection(initRegion, step.region)

    if (intersect) {
      acc.push({ on: step.on, region: intersect })
    }
    return acc
  }, [])

  return cubesOnInRegionAfterStep(steps, initRegion, steps.length - 1)
}

export const solvePart2 = (input: string) => {
  const steps = parseInput(input)
  const universeRegion = cuboidEnclosure(steps.map(({ region }) => region))

  return cubesOnInRegionAfterStep(steps, universeRegion, steps.length - 1)
}
