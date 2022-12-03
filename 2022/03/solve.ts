import { chunksOf, sum } from '@common'

const getItemPriority = (name: string): number => {
  const charCode = name.charCodeAt(0)
  return charCode > 96 ? charCode - 96 : charCode - 38
}

const parseInput = (input: string): number[][] => {
  return input
    .trim()
    .split('\n')
    .map((line) => line.split('').map(getItemPriority))
}

const getCommonItemInGroup = (group: number[][]): number => {
  const all = new Set(group.flat())
  const sets = group.map((items) => new Set(items))

  return Array.from(all.values()).find((item) =>
    sets.every((set) => set.has(item)),
  )!
}

const getMisplacedItem = (items: number[]): number => {
  const group = chunksOf(items.length / 2, items)

  return getCommonItemInGroup(group)
}

export const solvePart1 = (input: string): number => {
  const rucksacks = parseInput(input)

  return sum(rucksacks.map(getMisplacedItem))
}

export const solvePart2 = (input: string): number => {
  const rucksacks = parseInput(input)
  const groups = chunksOf(3, rucksacks)

  return sum(groups.map(getCommonItemInGroup))
}
