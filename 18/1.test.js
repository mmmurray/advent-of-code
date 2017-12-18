const execute = require('./1');

test('set instruction number', () => {
  const { registers } = execute('set a 1\nset b 2');

  expect(registers).toEqual({ a: 1, b: 2 });
});

test('set instruction register', () => {
  const { registers } = execute('set a 1\nset b a');

  expect(registers).toEqual({ a: 1, b: 1 });
});

test('add instruction', () => {
  const { registers } = execute('set a 1\nadd a 2');

  expect(registers).toEqual({ a: 3 });
});

test('mul instruction', () => {
  const { registers } = execute('set a 12\nmul a 3');

  expect(registers).toEqual({ a: 36 });
});

test('mod instruction', () => {
  const { registers } = execute('set a 12\nmod a 5');

  expect(registers).toEqual({ a: 2 });
});

test('snd instruction', () => {
  const { lastFrequency } = execute('set a 56\nsnd a');

  expect(lastFrequency).toBe(56);
});

test('jgz instruction', () => {
  const { registers } = execute('set a 2\nadd a -1\njgz a -1');

  expect(registers).toEqual({ a: 0 });
});

test('rcv instruction', () => {
  const { firstRecovered } = execute('set b 7\nset c 6\nsnd c\nset a 0\nrcv a\nsnd b\nset a 2\nrcv a');

  expect(firstRecovered).toBe(7);
});

test('puzzle', () => {
  const { firstRecovered } = execute(require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8'));

  expect(firstRecovered).toBe(3423);
});
