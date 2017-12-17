const spin = require('./2');

test('puzzle', () => {
  expect(spin(329)).toBe(27361412);
});
