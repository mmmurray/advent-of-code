import { pathFrom, readUTF8File } from 'core/file'
import { expect, test } from 'core/test'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', async () => {
  const input = await readUTF8File(pathFrom('./02/example.txt')).unwrap()

  expect(solvePart1(input)).toEqual(150)
})

test('part 1', async () => {
  const input = await readUTF8File(pathFrom('./02/input.txt')).unwrap()

  expect(solvePart1(input)).toEqual(2187380)
})

test('part 2 example', async () => {
  const input = await readUTF8File(pathFrom('./02/example.txt')).unwrap()

  expect(solvePart2(input)).toEqual(900)
})

test('part 2', async () => {
  const input = await readUTF8File(pathFrom('./02/input.txt')).unwrap()

  expect(solvePart2(input)).toEqual(2086357770)
})
