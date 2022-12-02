import { vec2, vec2Add } from 'core/math'

type Vec2 = [number, number]

const parseInput = (input: string): [Vec2, Vec2] => {
  const matches =
    /^target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)$/.exec(
      input.trim(),
    )!
  return [
    [Number(matches[1]), Number(matches[2])],
    [Number(matches[3]), Number(matches[4])],
  ]
}

const approachZero = (x: number): number => {
  if (x === 0) {
    return 0
  }

  return (Math.abs(x) - 1) * Math.sign(x)
}

const simulate = (
  initialVelocity: Vec2,
  steps: number,
  target: [Vec2, Vec2],
): { hit: boolean; maxY: number } => {
  const [[xMin, xMax], [yMin, yMax]] = target
  let position = vec2()
  let velocity = initialVelocity
  let hit = false
  let maxY = 0

  for (let step = 0; step < steps; step++) {
    position = vec2Add(position, velocity)
    velocity = vec2(approachZero(velocity[0]), velocity[1] - 1)
    maxY = Math.max(maxY, position[1])

    if (
      position[0] >= xMin &&
      position[0] <= xMax &&
      position[1] >= yMin &&
      position[1] <= yMax
    ) {
      hit = true
      break
    }
  }

  return { hit, maxY }
}

export const solvePart1 = (input: string) => {
  const target = parseInput(input)
  const steps = 250
  let maxY = 0

  for (let vx = 0; vx < 20; vx++) {
    for (let vy = 0; vy < 1000; vy++) {
      const velocity = vec2(vx, vy)
      const result = simulate(velocity, steps, target)
      if (result.hit) {
        maxY = Math.max(maxY, result.maxY)
      }
    }
  }

  return maxY
}

export const solvePart2 = (input: string) => {
  const target = parseInput(input)
  const steps = 250
  const uniqueVelocities = new Set<string>()

  for (let vx = 0; vx < 180; vx++) {
    for (let vy = -120; vy < 120; vy++) {
      const velocity = vec2(vx, vy)
      const result = simulate(velocity, steps, target)
      if (result.hit) {
        uniqueVelocities.add(velocity.join(','))
      }
    }
  }

  return uniqueVelocities.size
}
