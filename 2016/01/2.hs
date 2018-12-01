import qualified Data.List as List
import qualified Data.Set as Set
import Data.List.Split (splitOn)

orient :: (Int, Int) -> Char -> (Int, Int)
orient (dx, dy) rotate
  | rotate == 'R' = (-dy, dx)
  | rotate == 'L' = (dy, -dx)
  | otherwise = (dx, dy)

expandDirection :: (Char, Int) -> [(Char, Int)]
expandDirection (rotate, distance) = (rotate, 1) : (List.map (\x -> (' ', 1)) [2..distance])

walk :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
walk (x, y) (dx, dy) distance = (x + (dx * distance), y + (dy * distance))

tick :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
tick (position, direction) (turn, distance) =
  let
    newDirection = orient direction turn
    newPosition = walk position newDirection distance
  in
    (newPosition, newDirection)

follow :: [(Char, Int)] -> [(Int, Int)]
follow directions =
  let
    initial = ((0, 0), (0, -1))
    places = List.scanl tick initial directions
  in
    List.map (\(position, direction) -> position) places

parse :: String -> [(Char, Int)]
parse input = map tokenise (splitOn ", " input)
  where
    tokenise (d:digits) = (d, read digits)

distance :: (Int, Int) -> Int
distance (x, y) = (abs x) + (abs y)

getFirstDuplicate :: [(Int, Int)] -> (Int, Int)
getFirstDuplicate positions =
  go Set.empty positions
    where go seen (x:xs)
            | Set.member x seen = x
            | otherwise = go (Set.insert x seen) xs

main = do
  contents <- getContents
  let
    directions = parse contents
    expandedDirections = List.concatMap expandDirection directions
    visited = follow expandedDirections
    secondVisit = getFirstDuplicate visited
  print $ distance secondVisit
