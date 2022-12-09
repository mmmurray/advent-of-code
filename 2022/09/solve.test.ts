import { expect, test } from '@test'
import example1 from './example1.txt'
import example2 from './example2.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', () => {
  expect(solvePart1(example1)).toEqual(13)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(5513)
})

test('part 2 example', () => {
  expect(solvePart2(example1)).toEqual(1)
  expect(solvePart2(example2)).toEqual(36)
})

test('part 2', () => {
  expect(solvePart2(input)).toEqual(2427)
})
