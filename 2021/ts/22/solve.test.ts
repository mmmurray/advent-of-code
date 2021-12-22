import { expect, test } from 'core/test'
import example1 from './example-1.txt'
import example2 from './example-2.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', () => {
  expect(solvePart1(example1)).toEqual(590784)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(546724)
})

test('part 2 example', () => {
  expect(solvePart2(example2)).toEqual(2758514936282235)
})

test('part 2', () => {
  expect(solvePart2(input)).toEqual(1346544039176841)
})
