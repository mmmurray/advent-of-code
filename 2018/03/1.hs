import Text.Regex.PCRE
import Data.List
import qualified Data.Set as Set

regex = "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$"

type Square = (Int, Int, Int, Int, Int)
type Point = (Int, Int)

pointsInSquare :: Square -> Set.Set Point
pointsInSquare (id, x, y, w, h) =
  let
    x2 = x + w
    y2 = y + h
    points = [(x, y) | x <- [x..x2], y <- [y..y2]]
  in
    Set.fromList points

overlappingPoints :: (Square, Square) -> Set.Set Point
overlappingPoints ((id1, x11, y11, w1, h1), (id2, x21, y21, w2, h2)) =
  let
    x12 = x11 + w1 - 1
    x22 = x21 + w2 - 1
    y12 = y11 + h1 - 1
    y22 = y21 + h2 - 1
    x31 = max x11 x21
    x32 = min x12 x22
    y31 = max y11 y21
    y32 = min y12 y22
    w3 = x32 - x31
    h3 = y32 - y31
  in
    pointsInSquare (0, x31, y31, w3, h3)

parse :: String -> Square
parse line =
  let
    matches = line =~ regex :: [[String]]
    parts = tail $ head matches
    toTuple [id, x, y, w, h] = (id, x, y, w, h)
  in
    toTuple $ map read parts

main = do
  contents <- getContents
  let
    squares = map parse $ lines contents
    allPairs = [(x, y) | (x:ys) <- tails squares, y <- ys]
    allOverlapping = foldr (\s acc -> Set.union acc (overlappingPoints s)) Set.empty allPairs
  print $ length allOverlapping
