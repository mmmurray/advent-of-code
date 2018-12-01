const getCount = require('./1');

test('example1', () => {
  expect(getCount(65, 8921, 5)).toBe(1);
});

test('example2', () => {
  expect(getCount(65, 8921, 40000000)).toBe(588);
});

test('puzzle', () => {
  expect(getCount(703, 516, 40000000)).toBe(594);
});
