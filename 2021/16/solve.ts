type Packet =
  | {
      $: 'Literal'
      version: number
      value: number
    }
  | {
      $: 'Operator'
      version: number
      type: number
      packets: Packet[]
    }

type BitStream = {
  take: (n: number) => BitStream
  value: () => number
  offset: () => number
  bits: () => string
}

const createBitStream = (bits: string): BitStream => {
  let offset = 0

  return {
    take: (n) => {
      const slice = bits.slice(offset, offset + n)
      offset += n
      return createBitStream(slice)
    },
    value: () => {
      return parseInt(bits, 2)
    },
    offset: () => offset,
    bits: () => bits,
  }
}

export const parsePacket = (bits: BitStream): Packet => {
  const version = bits.take(3).value()
  const type = bits.take(3).value()

  if (type === 4) {
    let valueBits = ''
    let cont = 1

    while (cont === 1) {
      const group = bits.take(5)
      cont = group.take(1).value()
      const nibble = group.take(4).bits()
      valueBits += nibble
    }

    const value = createBitStream(valueBits).value()

    return { $: 'Literal', version, value }
  }

  const packets: Packet[] = []
  const lengthType = bits.take(1).value()

  if (lengthType === 0) {
    const lengthBits = bits.take(15).value()
    const initialOffset = bits.offset()

    while (bits.offset() < initialOffset + lengthBits) {
      packets.push(parsePacket(bits))
    }
  } else {
    const lengthPackets = bits.take(11).value()

    while (packets.length < lengthPackets) {
      packets.push(parsePacket(bits))
    }
  }

  return { $: 'Operator', version, type, packets }
}

export const parseInput = (hex: string): Packet => {
  const bits = hex
    .trim()
    .split('')
    .map((h) => parseInt(h, 16))
    .map((x) => x.toString(2).padStart(4, '0'))
    .join('')

  return parsePacket(createBitStream(bits))
}

const sumVersions = (packet: Packet): number => {
  if (packet.$ === 'Literal') {
    return packet.version
  }

  return (
    packet.version + packet.packets.reduce((acc, p) => acc + sumVersions(p), 0)
  )
}

const evaluatePacket = (packet: Packet): number => {
  if (packet.$ === 'Literal') {
    return packet.value
  }

  switch (packet.type) {
    case 0: {
      return packet.packets.reduce((acc, p) => acc + evaluatePacket(p), 0)
    }
    case 1: {
      return packet.packets.reduce((acc, p) => acc * evaluatePacket(p), 1)
    }
    case 2: {
      return Math.min(...packet.packets.map(evaluatePacket))
    }
    case 3: {
      return Math.max(...packet.packets.map(evaluatePacket))
    }
    case 5: {
      const [a, b] = packet.packets.map(evaluatePacket)
      return a > b ? 1 : 0
    }
    case 6: {
      const [a, b] = packet.packets.map(evaluatePacket)
      return a < b ? 1 : 0
    }
    case 7: {
      const [a, b] = packet.packets.map(evaluatePacket)
      return a === b ? 1 : 0
    }
    default: {
      throw new Error(`Unexpected packet type ${packet.type}`)
    }
  }
}

export const solvePart1 = (input: string) => {
  return sumVersions(parseInput(input))
}

export const solvePart2 = (input: string) => {
  return evaluatePacket(parseInput(input))
}
