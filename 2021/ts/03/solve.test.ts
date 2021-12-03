import { pathFrom, readUTF8File } from 'core/file'
import { expect, test } from 'core/test'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', async () => {
  const input = await readUTF8File(pathFrom('./03/example.txt')).unwrap()

  expect(solvePart1(input)).toEqual(198)
})

test('part 1', async () => {
  const input = await readUTF8File(pathFrom('./03/input.txt')).unwrap()

  expect(solvePart1(input)).toEqual(3958484)
})

test('part 2 example', async () => {
  const input = await readUTF8File(pathFrom('./03/example.txt')).unwrap()

  expect(solvePart2(input)).toEqual(230)
})

test('part 2', async () => {
  const input = await readUTF8File(pathFrom('./03/input.txt')).unwrap()

  expect(solvePart2(input)).toEqual(1613181)
})
