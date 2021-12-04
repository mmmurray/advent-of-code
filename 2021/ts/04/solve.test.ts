import { pathFrom, readUTF8File } from 'core/file'
import { expect, test } from 'core/test'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', async () => {
  const input = await readUTF8File(pathFrom('./04/example.txt')).unwrap()

  expect(solvePart1(input)).toEqual(4512)
})

test('part 1', async () => {
  const input = await readUTF8File(pathFrom('./04/input.txt')).unwrap()

  expect(solvePart1(input)).toEqual(46920)
})

test('part 2 example', async () => {
  const input = await readUTF8File(pathFrom('./04/example.txt')).unwrap()

  expect(solvePart2(input)).toEqual(1924)
})

test('part 2', async () => {
  const input = await readUTF8File(pathFrom('./04/input.txt')).unwrap()

  expect(solvePart2(input)).toEqual(12635)
})
