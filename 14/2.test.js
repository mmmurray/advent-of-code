const countRegions = require('./2');

test('example', () => {
  expect(countRegions('flqrgnkx')).toBe(1242);
});

test('puzzle', () => {
  expect(countRegions('hxtvlmkl')).toBe(1093);
});
