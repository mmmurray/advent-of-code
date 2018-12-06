import qualified Data.List.Split as Split
import Data.List

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = (abs $ x2 - x1) + (abs $ y2 - y1)

parseLine :: String -> (Int, Int)
parseLine line =
  let
    [x,y] = Split.splitOn ", " line
  in
    (read x, read y)

findClosestTo :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
findClosestTo points (tx, ty) =
  minimumBy (\(x1, y1) (x2, y2) -> compare (distance (x1, y1) (tx, ty)) (distance (x2, y2) (tx, ty))) points

main = do
  contents <- getContents
  let
    allLines = lines contents
    points = map parseLine allLines
    p = findClosestTo points (1,2)
  print $ points
  print p
