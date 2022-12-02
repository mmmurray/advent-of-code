import { array } from 'core/common'

type Cell = null | number

type Burrow = {
  hall: Cell[]
  rooms: Cell[][]
}

type Move = [Burrow, number]

const types = ['A', 'B', 'C', 'D']
const roomOffsets = [2, 4, 6, 8]
const movementEnergy = [1, 10, 100, 1000]

const parseRooms = (line: string): Cell[] => {
  return line
    .substring(3, 10)
    .split('#')
    .map((c) => types.indexOf(c))
    .map((type) => type)
}

const parseInput = (input: string): Burrow => {
  const lines = input.trim().split('\n')
  const roomsTop = parseRooms(lines[2])
  const roomsBottom = parseRooms(lines[3])

  return {
    hall: array<Cell>(11, null),
    rooms: [
      [roomsTop[0], roomsBottom[0]],
      [roomsTop[1], roomsBottom[1]],
      [roomsTop[2], roomsBottom[2]],
      [roomsTop[3], roomsBottom[3]],
    ],
  }
}

const isComplete = (burrow: Burrow): boolean => {
  return burrow.rooms.every(([r0, r1], index) => r0 === index && r1 === index)
}

const isHallOccupied = (burrow: Burrow, x1: number, x2: number): boolean => {
  const xMin = Math.min(x1, x2)
  const xMax = Math.max(x1, x2)
  for (let x = xMin; x <= xMax; x++) {
    if (burrow.hall[x] !== null) {
      return true
    }
  }
  return false
}

const cloneBurrow = (burrow: Burrow): Burrow => {
  return {
    hall: [...burrow.hall],
    rooms: burrow.rooms.map((r) => [...r]),
  }
}

const moveFromHallToRoom = (burrow: Burrow, h: number): Move | null => {
  const cell = burrow.hall[h]

  if (cell === null) {
    // Empty space in hall
    return null
  }

  const rooms = burrow.rooms[cell]

  if (rooms[0] !== null) {
    // Target room is full
    return null
  }

  if (rooms.some((r) => r !== null && r !== cell)) {
    // Target room contains other type
    return null
  }

  const x1 = roomOffsets[cell]
  const x2 = h + Math.sign(x1 - h)
  if (isHallOccupied(burrow, x1, x2)) {
    return null
  }

  const newBurrow = cloneBurrow(burrow)

  let row = 0
  while (rooms[row] === null) row++
  row--

  const distance = Math.abs(x2 - x1) + row + 2
  const energy = movementEnergy[cell] * distance

  newBurrow.hall[h] = null
  newBurrow.rooms[cell][row] = cell

  return [newBurrow, energy]
}

const moveFromRoomToRoom = (burrow: Burrow, col: number): Move | null => {
  const rooms = burrow.rooms[col]
  const row = rooms.findIndex((r) => r !== null)

  if (row === -1) {
    // Room is empty
    return null
  }

  if (rooms[row] === col) {
    // Room is complete
    return null
  }

  const cell = rooms[row]!

  if (!burrow.rooms[cell].every((c) => c === null || c === cell)) {
    // Cannot stack on top of different type
    return null
  }

  const x1 = roomOffsets[col]
  const x2 = roomOffsets[cell]
  if (isHallOccupied(burrow, x1, x2)) {
    return null
  }

  const targetRow = burrow.rooms[cell].findIndex((r) => r !== null)
  const distance = Math.abs(x2 - x1) + row + targetRow + 2
  const energy = movementEnergy[cell] * distance
  const newBurrow = cloneBurrow(burrow)
  newBurrow.rooms[col][row] = null
  newBurrow.rooms[cell][targetRow] = cell

  return [newBurrow, energy]
}

const moveFromRoomToHall = (
  burrow: Burrow,
  room: number,
  x: number,
): Move | null => {
  if (burrow.rooms[room].every((cell) => cell === null || cell === room)) {
    // Room has contains the correct types so don't move into hall
    return null
  }

  const y = burrow.rooms[room].findIndex((cell) => cell !== null)
  const cell = burrow.rooms[room][y]

  if (cell === null) {
    // Room is empty
    return null
  }

  if (roomOffsets.includes(x)) {
    return null
  }

  const x1 = x
  const x2 = roomOffsets[room]
  if (isHallOccupied(burrow, x1, x2)) {
    return null
  }

  const distance = Math.abs(x2 - x1) + y + 1
  const energy = movementEnergy[cell] * distance
  const newBurrow = cloneBurrow(burrow)
  newBurrow.hall[x] = cell
  newBurrow.rooms[room][y] = null

  return [newBurrow, energy]
}

const getAvailableMoves = (burrow: Burrow): Move[] => {
  const availableMoves: Move[] = []

  for (let i = 0; i < burrow.rooms.length; i++) {
    const move = moveFromRoomToRoom(burrow, i)
    if (move) {
      availableMoves.push(move)
    }

    for (let x = 0; x < burrow.hall.length; x++) {
      const move = moveFromRoomToHall(burrow, i, x)
      if (move) {
        availableMoves.push(move)
      }
    }
  }

  for (let x = 0; x < burrow.hall.length; x++) {
    const move = moveFromHallToRoom(burrow, x)
    if (move) {
      availableMoves.push(move)
    }
  }

  return availableMoves
}

const arrangeUsingLeastEnergy = (burrow: Burrow): number => {
  const cache = new Map<string, number>()

  const rec = (burrow: Burrow): number => {
    const key = JSON.stringify(burrow)
    const cached = cache.get(key)
    if (cached !== undefined) {
      return cached
    }

    if (isComplete(burrow)) {
      return 0
    }

    let energyMin: number = Infinity

    for (const [newBurrow, energyCost] of getAvailableMoves(burrow)) {
      energyMin = Math.min(energyMin, energyCost + rec(newBurrow))
    }

    cache.set(key, energyMin)

    return energyMin
  }

  return rec(burrow)
}

export const solvePart1 = (input: string) => {
  const burrow = parseInput(input)

  return arrangeUsingLeastEnergy(burrow)
}

export const solvePart2 = (input: string) => {
  const burrow = parseInput(input)

  burrow.rooms[0].splice(1, 0, 3, 3)
  burrow.rooms[1].splice(1, 0, 2, 1)
  burrow.rooms[2].splice(1, 0, 1, 0)
  burrow.rooms[3].splice(1, 0, 0, 2)

  return arrangeUsingLeastEnergy(burrow)
}
