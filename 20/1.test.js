const { readFileSync } = require('fs');
const simulate = require('./1');

test('puzzle', () => {
  const output = simulate(readFileSync(`${__dirname}/input.txt`, 'utf-8'));

  expect(output).toBe(161);
});
