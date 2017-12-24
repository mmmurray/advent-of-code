const { readFileSync } = require('fs');
const execute = require('./1');

test('puzzle', () => {
  const { mulTimes } = execute(readFileSync(`${__dirname}/input.txt`, 'utf-8'));

  expect(mulTimes).toBe(4225);
});
