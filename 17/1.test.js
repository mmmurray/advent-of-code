const spin = require('./1');

test('example', () => {
  expect(spin(3)).toBe(638);
});

test('puzzle', () => {
  expect(spin(329)).toBe(725);
});
