const distance = require('./1.js');

test('distance', () => {
  expect(distance(1)).toBe(0);
  expect(distance(2)).toBe(1);
  expect(distance(3)).toBe(2);
  expect(distance(4)).toBe(1);
  expect(distance(5)).toBe(2);
  expect(distance(6)).toBe(1);
  expect(distance(7)).toBe(2);
  expect(distance(8)).toBe(1);
  expect(distance(9)).toBe(2);
  expect(distance(10)).toBe(3);
  expect(distance(11)).toBe(2);
  expect(distance(12)).toBe(3);
  expect(distance(13)).toBe(4);
  expect(distance(14)).toBe(3);
  expect(distance(15)).toBe(2);
  expect(distance(16)).toBe(3);
  expect(distance(17)).toBe(4);
  expect(distance(18)).toBe(3);
  expect(distance(19)).toBe(2);
  expect(distance(20)).toBe(3);
  expect(distance(21)).toBe(4);
  expect(distance(22)).toBe(3);
  expect(distance(23)).toBe(2);
  expect(distance(24)).toBe(3);
  expect(distance(25)).toBe(4);
  expect(distance(26)).toBe(5);
  expect(distance(27)).toBe(4);

  expect(distance(1024)).toBe(31);

  expect(distance(368078)).toBe(371);
});
