const { hash2 } = require('./1+2');

test('examples', () => {
  expect(hash2('')).toBe('a2582a3a0e66e6e86e3812dcb672a272');
  expect(hash2('AoC 2017')).toBe('33efeb34ea91902bb2f59c9920caa6cd');
  expect(hash2('1,2,3')).toBe('3efbe78a8d82f29979031a4aa0b16a9d');
  expect(hash2('1,2,4')).toBe('63960835bcdc130f0b66d7ff4f6a5a8e');
});

test('puzzle', () => {
  expect(hash2('120,93,0,90,5,80,129,74,1,165,204,255,254,2,50,113')).toBe('d067d3f14d07e09c2e7308c3926605c4');
});
