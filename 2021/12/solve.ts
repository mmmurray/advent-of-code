type CaveId = string

type Caves = {
  edges: Map<CaveId, Set<CaveId>>
  smallCaveIds: Set<CaveId>
}

const parseInput = (input: string): Caves => {
  const caves: Caves = {
    edges: new Map(),
    smallCaveIds: new Set(),
  }

  const addEdge = (from: CaveId, to: CaveId) => {
    if (to === 'start') return
    const destinations = caves.edges.get(from) ?? new Set()
    destinations.add(to)
    caves.edges.set(from, destinations)
  }

  input
    .trim()
    .split('\n')
    .forEach((line) => {
      const [a, b] = line.split('-')
      addEdge(a, b)
      addEdge(b, a)

      if (a === a.toLowerCase()) {
        caves.smallCaveIds.add(a)
      }
      if (b === b.toLowerCase()) {
        caves.smallCaveIds.add(b)
      }
    })

  return caves
}

const removeCaveId = (set: Set<CaveId>, caveId: CaveId): Set<CaveId> => {
  const newSet = new Set(set)
  newSet.delete(caveId)
  return newSet
}

const countPaths = (caves: Caves, canVisitSmallTwice: boolean): number => {
  const rec = (
    from: CaveId,
    smallUnvisted: Set<CaveId>,
    canVisitSmallTwice: boolean,
  ): number => {
    if (from === 'end') {
      return 1
    }

    return Array.from(caves.edges.get(from)!.values()).reduce((acc, to) => {
      if (caves.smallCaveIds.has(to)) {
        if (smallUnvisted.has(to)) {
          return (
            acc + rec(to, removeCaveId(smallUnvisted, to), canVisitSmallTwice)
          )
        } else if (canVisitSmallTwice) {
          return acc + rec(to, smallUnvisted, false)
        } else {
          return acc
        }
      }

      return acc + rec(to, smallUnvisted, canVisitSmallTwice)
    }, 0)
  }

  return rec('start', caves.smallCaveIds, canVisitSmallTwice)
}

export const solvePart1 = (input: string) => {
  const caves = parseInput(input)

  return countPaths(caves, false)
}

export const solvePart2 = (input: string) => {
  const caves = parseInput(input)

  return countPaths(caves, true)
}
