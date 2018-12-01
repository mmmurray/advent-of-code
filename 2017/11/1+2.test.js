const steps = require('./1+2');

test('examples', () => {
  expect(steps('ne,ne,ne').distance).toBe(3);
  expect(steps('ne,ne,sw,sw').distance).toBe(0);
  expect(steps('ne,ne,s,s').distance).toBe(2);
  expect(steps('se,sw,se,sw,sw').distance).toBe(3);
});

test('single steps', () => {
  expect(steps('n').distance).toBe(1);
  expect(steps('s').distance).toBe(1);

  expect(steps('ne,s').distance).toBe(1);
  expect(steps('s,ne').distance).toBe(1);

  expect(steps('se,n').distance).toBe(1);
  expect(steps('n,se').distance).toBe(1);

  expect(steps('nw,s').distance).toBe(1);
  expect(steps('s,nw').distance).toBe(1);

  expect(steps('sw,n').distance).toBe(1);
  expect(steps('n,sw').distance).toBe(1);

  expect(steps('sw,se').distance).toBe(1);
  expect(steps('se,sw').distance).toBe(1);

  expect(steps('nw,ne').distance).toBe(1);
  expect(steps('ne,nw').distance).toBe(1);
});

test('double steps', () => {
  expect(steps('n,n').distance).toBe(2);
  expect(steps('ne,s,ne,s').distance).toBe(2);
  expect(steps('se,n,se,n').distance).toBe(2);
  expect(steps('s,s').distance).toBe(2);
  expect(steps('nw,s,nw,s').distance).toBe(2);
  expect(steps('sw,n,sw,n').distance).toBe(2);
});

test('puzzle', () => {
  const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');
  expect(steps(input)).toEqual({ distance: 794, max: 1524 });
});
