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

findClosestTo :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
findClosestTo points target =
  let
    distances = sort $ map (distance target) points
    equidistant = (head distances) == (head . tail $ distances)
  in
    if equidistant
      then (0, 0)
      else minimumBy (\(x1, y1) (x2, y2) -> compare (distance (x1, y1) target) (distance (x2, y2) target)) points

perimeterPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
perimeterPoints (x1, y1) (x2, y2) = nub $
  [(x, y1) | x <- [x1..x2]] ++
  [(x2, y) | y <- [y1..y2]] ++
  [(x, y2) | x <- [x1..x2]] ++
  [(x1, y) | y <- [y1..y2]]

areaPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
areaPoints (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

countPoints :: [(Int, Int)] -> (Int, Int) -> Int
countPoints points target = length $ filter (== target) points

main = do
  contents <- getContents
  let
    allLines = lines contents
    points = map parseLine allLines
    bound1 = (0, 0)
    bound2 = (500, 500)
    perimeter = perimeterPoints bound1 bound2
    area = areaPoints bound1 bound2
    infiniteRegions = nub $ map (findClosestTo points) perimeter
    voronoiPoints = map (findClosestTo points) area
    finiteVoronoiRegions = Set.toList $ Set.difference (Set.fromList voronoiPoints) (Set.fromList infiniteRegions)
    finiteVoronoiAreas = map (countPoints voronoiPoints) finiteVoronoiRegions
    largestArea = last . sort $ finiteVoronoiAreas
  print largestArea
