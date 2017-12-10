const { hash1 } = require('./1+2');

test('example', () => {
  expect(hash1(5, [3, 4, 1, 5])).toBe(12);
});

test('puzzle', () => {
  expect(hash1(256, [120, 93, 0, 90, 5, 80, 129, 74, 1, 165, 204, 255, 254, 2, 50, 113])).toBe(826);
});
