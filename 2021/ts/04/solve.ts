type Board = number[][]

type Input = {
  numbersToBeCalled: number[]
  boards: Board[]
}

const parseInput = (input: string): Input => {
  const [numbersToBeCalledSection, ...boardSections] = input.split('\n\n')

  const numbersToBeCalled = numbersToBeCalledSection
    .trim()
    .split(',')
    .map(Number)

  const boards = boardSections.map((boardSection) =>
    boardSection
      .trim()
      .split('\n')
      .map((row) => row.trim().split(/\s+/).map(Number)),
  )

  return {
    numbersToBeCalled,
    boards,
  }
}

const isWinningBoard = (board: Board, calledNumbers: number[]): boolean => {
  for (let row = 0; row < 5; row++) {
    let winning = true
    for (let col = 0; col < 5; col++) {
      if (!calledNumbers.includes(board[row][col])) {
        winning = false
      }
    }
    if (winning) {
      return true
    }
  }

  for (let col = 0; col < 5; col++) {
    let winning = true
    for (let row = 0; row < 5; row++) {
      if (!calledNumbers.includes(board[row][col])) {
        winning = false
      }
    }
    if (winning) {
      return true
    }
  }

  return false
}

const sum = (xs: number[]): number => xs.reduce<number>((acc, x) => acc + x, 0)

const last = (xs: number[]): number => xs[xs.length - 1]

const getUncalledBoardNumbers = (
  board: Board,
  calledNumbers: number[],
): number[] =>
  board
    .reduce<number[]>((acc, row) => [...acc, ...row], [])
    .filter((x) => !calledNumbers.includes(x))

const calculateBoardScore = (board: Board, calledNumbers: number[]): number =>
  sum(getUncalledBoardNumbers(board, calledNumbers)) * last(calledNumbers)

type GetBoardResult = (
  winningBoards: Board[],
  prevWinningBoards: Board[],
  boards: Board[],
) => Board | null

const playBingo = (
  numbersToBeCalled: number[],
  boards: Board[],
  getBoardResult: GetBoardResult,
): number => {
  const rec = (numbersCalled: number, prevWinningBoards: Board[]): number => {
    const calledNumbers = numbersToBeCalled.slice(0, numbersCalled)

    const winningBoards = boards.filter((board) =>
      isWinningBoard(board, calledNumbers),
    )

    const result = getBoardResult(winningBoards, prevWinningBoards, boards)

    return result
      ? calculateBoardScore(result, calledNumbers)
      : rec(numbersCalled + 1, winningBoards)
  }

  return rec(1, [])
}

const firstWinningBoard: GetBoardResult = (winningBoards) =>
  winningBoards.length > 0 ? winningBoards[0] : null

const lastWinningBoard: GetBoardResult = (
  winningBoards,
  prevWinningBoards,
  boards,
) =>
  winningBoards.length === boards.length
    ? winningBoards.find((b) => !prevWinningBoards.includes(b))!
    : null

export const solvePart1 = (input: string) => {
  const { numbersToBeCalled, boards } = parseInput(input)

  return playBingo(numbersToBeCalled, boards, firstWinningBoard)
}

export const solvePart2 = (input: string) => {
  const { numbersToBeCalled, boards } = parseInput(input)

  return playBingo(numbersToBeCalled, boards, lastWinningBoard)
}
