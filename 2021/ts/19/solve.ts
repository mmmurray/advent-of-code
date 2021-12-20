import {
  mat3,
  Mat3,
  vec3,
  Vec3,
  vec3Add,
  vec3Equal,
  vec3ManhattanDistance,
  vec3Sub,
  vec3Transform,
  vec3Zero,
} from 'core/math'

type Beacons = Vec3[]

type Scanner = {
  rotations: Beacons[]
}

type ScannerLocation = {
  offset: Vec3
  rotation: number
}

const rotationMatrices: Mat3[] = [
  mat3(1, 0, 0, 0, 1, 0, 0, 0, 1),
  mat3(1, 0, 0, 0, 0, -1, 0, 1, 0),
  mat3(1, 0, 0, 0, -1, 0, 0, 0, -1),
  mat3(1, 0, 0, 0, 0, 1, 0, -1, 0),
  mat3(0, -1, 0, 1, 0, 0, 0, 0, 1),
  mat3(0, 0, 1, 1, 0, 0, 0, 1, 0),
  mat3(0, 1, 0, 1, 0, 0, 0, 0, -1),
  mat3(0, 0, -1, 1, 0, 0, 0, -1, 0),
  mat3(-1, 0, 0, 0, -1, 0, 0, 0, 1),
  mat3(-1, 0, 0, 0, 0, -1, 0, -1, 0),
  mat3(-1, 0, 0, 0, 1, 0, 0, 0, -1),
  mat3(-1, 0, 0, 0, 0, 1, 0, 1, 0),
  mat3(0, 1, 0, -1, 0, 0, 0, 0, 1),
  mat3(0, 0, 1, -1, 0, 0, 0, -1, 0),
  mat3(0, -1, 0, -1, 0, 0, 0, 0, -1),
  mat3(0, 0, -1, -1, 0, 0, 0, 1, 0),
  mat3(0, 0, -1, 0, 1, 0, 1, 0, 0),
  mat3(0, 1, 0, 0, 0, 1, 1, 0, 0),
  mat3(0, 0, 1, 0, -1, 0, 1, 0, 0),
  mat3(0, -1, 0, 0, 0, -1, 1, 0, 0),
  mat3(0, 0, -1, 0, -1, 0, -1, 0, 0),
  mat3(0, -1, 0, 0, 0, 1, -1, 0, 0),
  mat3(0, 0, 1, 0, 1, 0, -1, 0, 0),
  mat3(0, 1, 0, 0, 0, -1, -1, 0, 0),
]

const parseInput = (input: string): Scanner[] => {
  const sections = input.trim().split('\n\n')

  return sections.map((section): Scanner => {
    const [, ...vectors] = section.split('\n')
    const beacons = vectors.map((vector) => {
      const [x, y, z] = vector.split(',').map(Number)
      return vec3(x, y, z)
    })

    const rotations: Beacons[] = rotationMatrices.map((m) => {
      return beacons.map((b) => vec3Transform(b, m))
    })

    return { rotations }
  })
}

const alignScanners = (
  s1Beacons: Beacons,
  s2: Scanner,
): ScannerLocation | null => {
  for (let i1 = 0; i1 < s1Beacons.length; i1++) {
    for (let rotation = 0; rotation < rotationMatrices.length; rotation++) {
      const s2Beacons = s2.rotations[rotation]
      for (let i2 = 0; i2 < s2Beacons.length; i2++) {
        const b1 = s1Beacons[i1]
        const b2 = s2Beacons[i2]
        const offset = vec3Sub(b1, b2)

        let aligned = 0
        for (let i3 = 0; i3 < s2Beacons.length; i3++) {
          if (
            s1Beacons.some((b) => vec3Equal(b, vec3Add(s2Beacons[i3], offset)))
          ) {
            aligned++
          }
          if (aligned === 12) {
            return { offset, rotation }
          }
        }
      }
    }
  }

  return null
}

const locateScanners = (scanners: Scanner[]): ScannerLocation[] => {
  const locations = new Map<number, ScannerLocation>()
  locations.set(0, { offset: vec3Zero(), rotation: 0 })

  const locateScanner = (scannerIndex: number): ScannerLocation | null => {
    for (const [i, other] of Array.from(locations.entries())) {
      const relativeLocation = alignScanners(
        scanners[i].rotations[other.rotation],
        scanners[scannerIndex],
      )

      if (relativeLocation) {
        return {
          offset: vec3Add(other.offset, relativeLocation.offset),
          rotation: relativeLocation.rotation,
        }
      }
    }

    return null
  }

  while (locations.size < scanners.length) {
    for (let i = 0; i < scanners.length; i++) {
      if (!locations.has(i)) {
        const location = locateScanner(i)
        if (location) {
          locations.set(i, location)
        }
      }
    }
  }

  return Array.from(locations.entries())
    .sort(([i1], [i2]) => i1 - i2)
    .map(([, l]) => l)
}

export const solvePart1 = (input: string) => {
  const scanners = parseInput(input)
  const locations = locateScanners(scanners)
  const beacons = locations
    .map((location, index) => {
      return scanners[index].rotations[location.rotation].map((b) =>
        vec3Add(b, location.offset),
      )
    })
    .flat()

  const uniqueBeacons = beacons.reduce(
    (acc, b) => acc.add(b.join(',')),
    new Set<string>(),
  )

  return uniqueBeacons.size
}

export const solvePart2 = (input: string) => {
  const scanners = parseInput(input)
  const locations = locateScanners(scanners)
  let maxDistance = 0

  for (const l1 of locations) {
    for (const l2 of locations) {
      const distance = vec3ManhattanDistance(l1.offset, l2.offset)
      maxDistance = Math.max(maxDistance, distance)
    }
  }

  return maxDistance
}
