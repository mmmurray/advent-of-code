const { readFileSync } = require('fs');
const build = require('./1+2');

test('example', () => {
  const { strongest, strongestLongest } = build(`0/2
  2/2
  2/3
  3/4
  3/5
  0/1
  10/1
  9/10`);

  expect(strongest).toBe(31);
  expect(strongestLongest).toBe(19);
});

test('puzzle', () => {
  const { strongest, strongestLongest } = build(readFileSync(`${__dirname}/input.txt`, 'utf-8'));

  expect(strongest).toBe(1511);
  expect(strongestLongest).toBe(1471);
});
