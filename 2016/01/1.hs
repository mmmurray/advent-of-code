import qualified Data.List as List
import Data.List.Split (splitOn)

orient :: (Int, Int) -> Char -> (Int, Int)
orient (dx, dy) rotate
  | rotate == 'R' = (-dy, dx)
  | rotate == 'L' = (dy, -dx)

walk :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
walk (x, y) (dx, dy) distance = (x + (dx * distance), y + (dy * distance))

tick :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
tick (position, direction) (turn, distance) =
  let
    newDirection = orient direction turn
    newPosition = walk position newDirection distance
  in
    (newPosition, newDirection)

follow :: [(Char, Int)] -> (Int, Int)
follow directions =
  let
    initial = ((0, 0), (0, -1))
    (position, direction) = List.foldl tick initial directions
  in
    position

parse :: String -> [(Char, Int)]
parse input = map tokenise (splitOn ", " input)
  where
    tokenise (d:digits) = (d, read digits)

distance :: (Int, Int) -> Int
distance (x, y) = (abs x) + (abs y)

main = do
  contents <- getContents
  let
    directions = parse contents
    destination = follow directions
  print $ distance destination
