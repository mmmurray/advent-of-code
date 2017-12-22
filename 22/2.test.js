const { readFileSync } = require('fs');
const infect = require('./2');

test('example 1', () => {
  const infected = infect('..#\n#..\n...', 100);

  expect(infected).toBe(26);
});

// Passes but take like 3 hours, so... yeah...
// test('puzzle', () => {
//   const infected = infect(readFileSync(`${__dirname}/input.txt`, 'utf-8'), 10000);

//   expect(infected).toBe(2511345);
// });
