import { expect, test } from '@test'
import example from './example.txt'
import input from './input.txt'
import { solvePart1 } from './solve'

test('part 1 example', () => {
  expect(solvePart1(example)).toEqual(58)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(520)
})
