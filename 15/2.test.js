const getCount = require('./2');

test('example1', () => {
  expect(getCount(65, 8921, 1055)).toBe(0);
  expect(getCount(65, 8921, 1056)).toBe(1);
});

test('example2', () => {
  expect(getCount(65, 8921, 5000000)).toBe(309);
});

test('puzzle', () => {
  expect(getCount(703, 516, 5000000)).toBe(328);
});
