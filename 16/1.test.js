const dance = require('./1');

test('spin', () => {
  expect(dance('abcde', 's1')).toBe('eabcd');
});

test('exchange', () => {
  expect(dance('abcde', 'x2/4')).toBe('abedc');
});

test('partner', () => {
  expect(dance('abcde', 'pa/d')).toBe('dbcae');
});

test('example', () => {
  expect(dance('abcde', 's1,x3/4,pe/b')).toBe('baedc');
});

test('puzzle', () => {
  expect(dance('abcdefghijklmnop', require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8'))).toBe(
    'gkmndaholjbfcepi'
  );
});
