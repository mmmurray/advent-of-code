import qualified Data.List.Split as Split
import qualified Data.Set as Set
import Data.List

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = (abs $ x2 - x1) + (abs $ y2 - y1)

parseLine :: String -> (Int, Int)
parseLine line =
  let
    [x,y] = Split.splitOn ", " line
  in
    (read x, read y)

totalDistanceTo :: [(Int, Int)] -> (Int, Int) -> Int
totalDistanceTo points target = sum $ map (distance target) points

areaPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
areaPoints (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

main = do
  contents <- getContents
  let
    allLines = lines contents
    points = map parseLine allLines
    area = areaPoints (0, 0) (500, 500)
    distances = map (totalDistanceTo points) area
    distancesWithPoints = zip distances area
    pointsInRegion = filter (\(d, p) -> d < 10000) distancesWithPoints
  print $ length pointsInRegion
