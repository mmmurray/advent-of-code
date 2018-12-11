import qualified Data.Array as Array
import qualified Data.List as List
import Debug.Trace

type Grid = Array.Array (Int, Int) Int
type Cell = (Int, Int)

gridSize = 300
serial = 3463
-- squareSize = 3

allSquaresInGrid :: Int -> [Cell]
allSquaresInGrid squareSize = [(x, y) | x <- [1..(gridSize - squareSize + 1)], y <- [1..(gridSize - squareSize + 1)]]


cellsInSquare :: Int -> Cell -> [Cell]
cellsInSquare squareSize (x, y) = [(sx, sy) | sx <- [x..(x + squareSize - 1)], sy <- [y..(y + squareSize - 1)]]


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


powerForSquare :: Grid -> Int -> Int -> Cell -> Int
powerForSquare powerLevels serial squareSize square =
  sum $ map (\c -> powerLevels Array.! c) (cellsInSquare squareSize square)


getLargestPower :: Grid -> Int -> Int -> (Cell, Int)
getLargestPower powerLevels squareSize serial =
  let
    squares = allSquaresInGrid squareSize
    powerForSquares = map (\cell -> (cell, powerForSquare powerLevels serial squareSize cell)) squares
    mostPower = List.maximumBy (\(c1, p1) (c2, p2) -> compare p1 p2) powerForSquares
  in
    mostPower


getPowerForSquareSizes :: Grid -> Int -> [((Cell, Int), Int)]
getPowerForSquareSizes powerLevels serial =
  let
    sizes = [1..gridSize]
  in
    map (\size -> ((getLargestPower powerLevels size serial), (traceShowId size))) sizes


getBestSquareSize :: Grid -> Int -> ((Cell, Int), Int)
getBestSquareSize powerLevels serial =
  let
    powerForSquareSizes = getPowerForSquareSizes powerLevels serial
    bestSquare = List.maximumBy (\(p1, s1) (p2, s2) -> compare (snd p1) (snd p2)) powerForSquareSizes
  in
    bestSquare


main = do
  let
    powerLevels = Array.array ((1, 1), (gridSize, gridSize)) [((x, y), powerForCell serial (x, y)) | x <- [1..gridSize], y <- [1..gridSize]]
    powerForSquareSizes = getPowerForSquareSizes powerLevels serial
    bestSquare = List.maximumBy (\(p1, s1) (p2, s2) -> compare (snd p1) (snd p2)) powerForSquareSizes
  print powerForSquareSizes
  print bestSquare
  -- print $ getPowerForSquareSizes powerLevels serial
    -- powerLevels = Array.array ((1,1),(2,2)) [((2,1),"C"),((1,2),"B"),((1,1),"A"),((2,2),"D")]
  -- print $ powerLevels
  -- print $ "Part 1: " ++ (show . fst $ getLargestPower powerLevels 3 serial)
  -- print $ "Part 2: " ++ (show $ getBestSquareSize powerLevels serial)
  -- print $ map (\size -> ((getLargestPower powerLevels size serial), (traceShowId size))) [0..20]
