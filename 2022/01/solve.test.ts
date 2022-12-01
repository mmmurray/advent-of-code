import { expect, test } from '@test'
import example from './example.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', () => {
  expect(solvePart1(example)).toEqual(24000)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(71934)
})

test('part 2 example', () => {
  expect(solvePart2(example)).toEqual(45000)
})

test('part 2', () => {
  expect(solvePart2(input)).toEqual(211447)
})
