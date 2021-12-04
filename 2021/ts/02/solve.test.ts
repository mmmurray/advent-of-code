import { expect, test } from 'core/test'
import example from './example.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', async () => {
  expect(solvePart1(example)).toEqual(150)
})

test('part 1', async () => {
  expect(solvePart1(input)).toEqual(2187380)
})

test('part 2 example', async () => {
  expect(solvePart2(example)).toEqual(900)
})

test('part 2', async () => {
  expect(solvePart2(input)).toEqual(2086357770)
})
