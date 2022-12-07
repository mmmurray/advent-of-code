import { sum } from '@common'

type OutputLine =
  | {
      $: 'cd'
      target: string
    }
  | {
      $: 'ls'
    }
  | {
      $: 'dir'
      name: string
    }
  | {
      $: 'file'
      name: string
      size: number
    }

type File = {
  $: 'file'
  name: string
  size: number
}

type Dir = {
  $: 'dir'
  name: string
  items: (Dir | File)[]
}

const convertOutputLinesToDir = (outputLines: OutputLine[]): Dir => {
  const rootDir: Dir = { $: 'dir', name: '/', items: [] }
  const dirStack: Dir[] = [{ $: 'dir', name: '', items: [rootDir] }]

  outputLines.forEach((line) => {
    const currentDir = dirStack[dirStack.length - 1]

    switch (line.$) {
      case 'cd': {
        if (line.target === '..') {
          dirStack.pop()
        } else {
          const targetDir = currentDir.items.find(
            (item): item is Dir =>
              item.$ === 'dir' && item.name === line.target,
          )

          if (!targetDir) {
            throw new Error(`Cannot cd into ${line.target}`)
          }

          dirStack.push(targetDir)
        }
        break
      }
      case 'ls': {
        break
      }
      case 'dir': {
        currentDir.items.push({ $: 'dir', name: line.name, items: [] })
        break
      }
      case 'file': {
        currentDir.items.push({ $: 'file', name: line.name, size: line.size })
        break
      }
    }
  })

  return rootDir
}

const parseInput = (input: string): Dir => {
  const outputLines: OutputLine[] = input
    .trim()
    .split('\n')
    .map((line) => {
      const cdMatches = /^\$ cd (.+)$/.exec(line)
      if (cdMatches) {
        return { $: 'cd', target: cdMatches[1] }
      }

      const lsMatches = /^\$ ls$/.exec(line)
      if (lsMatches) {
        return { $: 'ls' }
      }

      const dirMatches = /^dir (.+)$/.exec(line)
      if (dirMatches) {
        return { $: 'dir', name: dirMatches[1] }
      }

      const fileMatches = /^(\d+) (.+)$/.exec(line)!
      return { $: 'file', name: fileMatches[2], size: Number(fileMatches[1]) }
    })

  return convertOutputLinesToDir(outputLines)
}

const getDirSize = (dir: Dir): number => {
  return sum(
    dir.items.map((item) => {
      switch (item.$) {
        case 'dir': {
          return getDirSize(item)
        }
        case 'file': {
          return item.size
        }
      }
    }),
  )
}

const getDirSizes = (dir: Dir): number[] => {
  const itemResults = dir.items.flatMap((item) =>
    item.$ === 'dir' ? getDirSizes(item) : [],
  )

  return [getDirSize(dir), ...itemResults]
}

export const solvePart1 = (input: string): number => {
  const rootDir = parseInput(input)
  const smallDirSizes = getDirSizes(rootDir).filter((size) => size <= 100000)

  return sum(smallDirSizes)
}

export const solvePart2 = (input: string): number => {
  const rootDir = parseInput(input)
  const totalSize = getDirSize(rootDir)
  const freeSpace = totalSize - 40000000

  return getDirSizes(rootDir)
    .filter((size) => size >= freeSpace)
    .sort((a, b) => a - b)[0]
}
