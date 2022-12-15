import { expect, test } from '@test';
import example from './example.txt';
import input from './input.txt';
import { solvePart1, solvePart2 } from './solve';

test('part 1 example', () => {
  expect(solvePart1(example, 10)).toEqual(26);
});

test('part 1', () => {
  expect(solvePart1(input, 2000000)).toEqual(4961647);
});

test('part 2 example', () => {
  expect(solvePart2(example, 20)).toEqual(56000011);
});

test('part 2', () => {
  expect(solvePart2(input, 4000000)).toEqual(12274327017867);
});
