import { expect, test } from 'core/test'
import example from './example.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', async () => {
  expect(solvePart1(example)).toEqual(198)
})

test('part 1', async () => {
  expect(solvePart1(input)).toEqual(3958484)
})

test('part 2 example', async () => {
  expect(solvePart2(example)).toEqual(230)
})

test('part 2', async () => {
  expect(solvePart2(input)).toEqual(1613181)
})
