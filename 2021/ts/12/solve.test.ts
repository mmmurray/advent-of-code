import { expect, test } from 'core/test'
import example1 from './example-1.txt'
import example2 from './example-2.txt'
import example3 from './example-3.txt'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example 1', () => {
  expect(solvePart1(example1)).toEqual(10)
})

test('part 1 example 2', () => {
  expect(solvePart1(example2)).toEqual(19)
})

test('part 1 example 3', () => {
  expect(solvePart1(example3)).toEqual(226)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(4304)
})

test('part 2 example 1', () => {
  expect(solvePart2(example1)).toEqual(36)
})

test('part 2 example 2', () => {
  expect(solvePart2(example2)).toEqual(103)
})

test('part 2 example 3', () => {
  expect(solvePart2(example3)).toEqual(3509)
})

test('part 2', () => {
  expect(solvePart2(input)).toEqual(118242)
})
