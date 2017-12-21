const { readFileSync } = require('fs');
const { iterateTimes, iterateCount } = require('./1');

test('example', () => {
  const input = `
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
`;

  const result1 = iterateTimes(input);

  expect(result1).toBe('#..#\n....\n....\n#..#');

  const result2 = iterateTimes(input, 1, result1);

  expect(result2).toBe(`##.##.
#..#..
......
##.##.
#..#..
......`);
});

test('example count', () => {
  const input = `
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
`;

  expect(iterateCount(input, 2)).toBe(12);
});

test('rotate 2x2', () => {
  expect(iterateTimes('../.# => ##./#../...', 1, '..\n.#')).toBe('##.\n#..\n...');
  expect(iterateTimes('../.# => ##./#../...', 1, '..\n#.')).toBe('##.\n#..\n...');
  expect(iterateTimes('../.# => ##./#../...', 1, '#.\n..')).toBe('##.\n#..\n...');
  expect(iterateTimes('../.# => ##./#../...', 1, '.#\n..')).toBe('##.\n#..\n...');
});

test('rotate 3x3', () => {
  expect(iterateTimes('.#./..#/### => #..#/..../..../#..#', 1, '.#.\n..#\n###')).toBe('#..#\n....\n....\n#..#');
  expect(iterateTimes('.#./..#/### => #..#/..../..../#..#', 1, '#..\n#.#\n##.')).toBe('#..#\n....\n....\n#..#');
  expect(iterateTimes('.#./..#/### => #..#/..../..../#..#', 1, '###\n#..\n.#.')).toBe('#..#\n....\n....\n#..#');
  expect(iterateTimes('.#./..#/### => #..#/..../..../#..#', 1, '.##\n#.#\n..#')).toBe('#..#\n....\n....\n#..#');
});

test('flip 3x3', () => {
  expect(iterateTimes('.#./..#/### => #..#/..../..../#..#', 1, '.#.\n#..\n###')).toBe('#..#\n....\n....\n#..#');
  expect(iterateTimes('.#./..#/### => #..#/..../..../#..#', 1, '###\n..#\n.#.')).toBe('#..#\n....\n....\n#..#');
});

test('puzzle 1', () => {
  const input = readFileSync(`${__dirname}/input.txt`, 'utf-8');

  expect(iterateCount(input, 5)).toBe(164);
});

test('puzzle 2', () => {
  const input = readFileSync(`${__dirname}/input.txt`, 'utf-8');

  expect(iterateCount(input, 18)).toBe(2355110);
});
