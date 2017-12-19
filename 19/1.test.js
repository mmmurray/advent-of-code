const follow = require('./1');
const { readFileSync } = require('fs');

test('example', () => {
  const map = readFileSync(__dirname + '/example.txt', 'utf-8');

  expect(follow(map)).toEqual({ letters: 'ABCDEF', steps: 38 });
});

test('puzzle', () => {
  const map = readFileSync(__dirname + '/input.txt', 'utf-8');

  expect(follow(map)).toEqual({ letters: 'BPDKCZWHGT', steps: 17728 });
});
