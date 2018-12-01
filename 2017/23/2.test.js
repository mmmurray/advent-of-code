const { readFileSync } = require('fs');
const execute = require('./2');

test('puzzle', () => {
  const h = execute();

  expect(h).toBe(905);
});
