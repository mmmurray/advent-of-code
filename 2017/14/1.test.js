const countSquares = require('./1');

test('example', () => {
  expect(countSquares('flqrgnkx')).toBe(8108);
});

test('puzzle', () => {
  expect(countSquares('hxtvlmkl')).toBe(8214);
});
