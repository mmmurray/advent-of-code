import qualified Data.List as List

type Cell = (Int, Int)

gridSize = 300
squareSize = 3
allSquaresInGrid = [(x, y) | x <- [1..(gridSize - squareSize + 1)], y <- [1..(gridSize - squareSize + 1)]]


cellsInSquare :: Cell -> [Cell]
cellsInSquare (x, y) = [(sx, sy) | sx <- [x..(x + squareSize - 1)], sy <- [y..(y + squareSize - 1)]]


powerForCell :: Int -> Cell -> Int
powerForCell serial (x, y) =
  let
    rackId = x + 10
    value = ((rackId * y) + serial) * rackId
    digits = (drop 2) . reverse . show $ value
    hundreds = if length digits == 0 then 0 else read [head digits]
    power = hundreds - 5
  in
    power


powerForSquare :: Int -> Cell -> Int
powerForSquare serial square =
  sum $ map (powerForCell serial) (cellsInSquare square)


main = do
  let
    serial = 3463
    powerForSquares = map (\cell -> (cell, powerForSquare serial cell)) allSquaresInGrid
    mostPower = List.maximumBy (\(c1, p1) (c2, p2) -> compare p1 p2) powerForSquares
  print mostPower
