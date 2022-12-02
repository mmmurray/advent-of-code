type Vec2 = [number, number]

type Vec3 = [number, number, number]

const vec2Scale = ([x, y]: Vec2, s: number): Vec2 => [x * s, y * s]

const vec2Add = ([x1, y1]: Vec2, [x2, y2]: Vec2): Vec2 => [x1 + x2, y1 + y2]

const vec3Add = ([x1, y1, z1]: Vec3, [x2, y2, z2]: Vec3): Vec3 => [
  x1 + x2,
  y1 + y2,
  z1 + z2,
]

const directions: { [direction: string]: Vec2 } = {
  forward: [1, 0],
  down: [0, 1],
  up: [0, -1],
}

const parseInput = (input: string): Vec2[] =>
  input
    .trim()
    .split('\n')
    .map((line) => {
      const [direction, distance] = line.split(' ')

      return vec2Scale(directions[direction], Number(distance))
    })

export const solvePart1 = (input: string) => {
  const movements = parseInput(input)

  const final = movements.reduce<Vec2>(
    (acc, movement) => vec2Add(acc, movement),
    [0, 0],
  )

  return final[0] * final[1]
}

export const solvePart2 = (input: string) => {
  const movements = parseInput(input)

  const final = movements.reduce<Vec3>(
    (acc, movement) =>
      vec3Add(acc, [movement[0], movement[0] * acc[2], movement[1]]),
    [0, 0, 0],
  )

  return final[0] * final[1]
}
