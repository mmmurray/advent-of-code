import { expect, test } from 'core/test'
import input from './input.txt'
import { solvePart1, solvePart2 } from './solve'

test('part 1 example', () => {
  expect(solvePart1('8A004A801A8002F478')).toEqual(16)
  expect(solvePart1('620080001611562C8802118E34')).toEqual(12)
  expect(solvePart1('C0015000016115A2E0802F182340')).toEqual(23)
  expect(solvePart1('A0016C880162017C3686B18A3D4780')).toEqual(31)
})

test('part 1', () => {
  expect(solvePart1(input)).toEqual(847)
})

test('part 2 example', () => {
  expect(solvePart2('C200B40A82')).toEqual(3)
  expect(solvePart2('04005AC33890')).toEqual(54)
  expect(solvePart2('880086C3E88112')).toEqual(7)
  expect(solvePart2('CE00C43D881120')).toEqual(9)
  expect(solvePart2('D8005AC2A8F0')).toEqual(1)
  expect(solvePart2('F600BC2D8F')).toEqual(0)
  expect(solvePart2('9C005AC2F8F0')).toEqual(0)
  expect(solvePart2('9C0141080250320F1802104A08')).toEqual(1)
})

test('part 2', () => {
  expect(solvePart2(input)).toEqual(333794664059)
})
