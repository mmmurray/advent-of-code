import { array } from '@common'
import { vec2, Vec2, vec2Add, vec2Sub } from '@math'

const directionMap: { [direction: string]: Vec2 } = {
  U: vec2(0, 1),
  D: vec2(0, -1),
  R: vec2(1, 0),
  L: vec2(-1, 0),
}

const parseInput = (input: string): Vec2[] => {
  return input
    .trim()
    .split('\n')
    .flatMap((line) => {
      const [direction, distance] = line.split(' ')
      return array(Number(distance), directionMap[direction])
    })
}

const tailFollowHead = (head: Vec2, tail: Vec2): Vec2 => {
  const [dx, dy] = vec2Sub(head, tail)

  if (Math.abs(dx) > 1 || Math.abs(dy) > 1) {
    return vec2Add(tail, vec2(Math.sign(dx), Math.sign(dy)))
  }

  return tail
}

const solve = (input: string, ropeLength: number): number => {
  const steps = parseInput(input)
  const rope = array(ropeLength, vec2(0, 0))
  const tailPositions = new Set<string>()

  for (const step of steps) {
    rope[0] = vec2Add(rope[0], step)

    for (let i = 1; i < rope.length; i++) {
      rope[i] = tailFollowHead(rope[i - 1], rope[i])
    }

    tailPositions.add(rope[rope.length - 1].join(','))
  }

  return tailPositions.size
}

export const solvePart1 = (input: string): number => {
  return solve(input, 2)
}

export const solvePart2 = (input: string): number => {
  return solve(input, 10)
}
