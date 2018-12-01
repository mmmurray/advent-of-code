const dance = require('./2');

test('puzzle', () => {
  expect(dance('abcdefghijklmnop', require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8'))).toBe(
    'abihnfkojcmegldp'
  );
});
