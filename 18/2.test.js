const execute = require('./2');

test('puzzle', () => {
  const sent = execute(require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8'));

  expect(sent).toBe(7493);
});
