const { readFileSync } = require('fs');
const infect = require('./1');

test('example 1', () => {
  const infected = infect('..#\n#..\n...', 7);

  expect(infected).toBe(5);
});

test('example 2', () => {
  const infected = infect('..#\n#..\n...', 70);

  expect(infected).toBe(41);
});

test('example 3', () => {
  const infected = infect('..#\n#..\n...', 10000);

  expect(infected).toBe(5587);
});

test('puzzle', () => {
  const infected = infect(readFileSync(`${__dirname}/input.txt`, 'utf-8'), 10000);

  expect(infected).toBe(5256);
});
