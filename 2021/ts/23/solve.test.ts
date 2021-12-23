import { expect, test } from 'core/test'
import example from './example.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', () => {
  expect(solvePart1(example)).toEqual(12521)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(18300)
})

test('part 2 example', () => {
  expect(solvePart2(example)).toEqual(44169)
})

test('part 2', () => {
  expect(solvePart2(input)).toEqual(50190)
})
