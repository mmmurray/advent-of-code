const findEndOfUniqueMarker = (markerLength: number, input: string): number => {
  const symbols = input.trim().split('')

  for (let i = 0; i < symbols.length - markerLength; i++) {
    const marker = symbols.slice(i, i + markerLength)

    if (new Set(marker).size === markerLength) {
      return i + markerLength
    }
  }

  return -1
}

export const solvePart1 = (input: string): number => {
  return findEndOfUniqueMarker(4, input)
}

export const solvePart2 = (input: string): number => {
  return findEndOfUniqueMarker(14, input)
}
