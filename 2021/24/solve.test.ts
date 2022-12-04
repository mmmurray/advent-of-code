import { expect, test } from '@test'
import example1 from './example-1.txt'
import example2 from './example-2.txt'
import example3 from './example-3.txt'
import input from './input.txt'
import { execute, parseInput, solvePart1, solvePart2 } from './solve'

test('execute example 1', () => {
  expect(execute(parseInput(example1), [10])).toEqual({
    w: 0,
    x: -10,
    y: 0,
    z: 0,
  })
})

test('execute example 2', () => {
  expect(execute(parseInput(example2), [2, 3])).toEqual({
    w: 0,
    x: 3,
    y: 0,
    z: 0,
  })

  expect(execute(parseInput(example2), [2, 6])).toEqual({
    w: 0,
    x: 6,
    y: 0,
    z: 1,
  })
})

test('execute example 3', () => {
  expect(execute(parseInput(example3), [0])).toEqual({
    w: 0,
    x: 0,
    y: 0,
    z: 0,
  })
  expect(execute(parseInput(example3), [256])).toEqual({
    w: 0,
    x: 0,
    y: 0,
    z: 0,
  })
  expect(execute(parseInput(example3), [255])).toEqual({
    w: 1,
    x: 1,
    y: 1,
    z: 1,
  })
  expect(execute(parseInput(example3), [22])).toEqual({
    w: 0,
    x: 1,
    y: 1,
    z: 0,
  })
})

test.skip('part 1', () => {
  expect(solvePart1(input)).toEqual('91297395919993')
})

test.skip('part 2', () => {
  expect(solvePart2(input)).toEqual('71131151917891')
})
