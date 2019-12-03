import Data.List
import Data.List.Split

type Point = (Int, Int)

data Move = MoveRight Int
          | MoveUp Int
          | MoveLeft Int
          | MoveDown Int
              deriving (Eq, Show)

distance :: Point -> Int
distance (x, y) = abs x + abs y

closestCommon :: [Point] -> [Point] -> Int
closestCommon (o1 : as) (o2 : bs) = head $ sort $ map distance $ intersect as bs

indexOf :: [Point] -> Point -> Int
indexOf points point
  = case elemIndex point points of
        Nothing -> 0
        Just index -> index

shortestCommon :: [Point] -> [Point] -> Int
shortestCommon (o1 : as) (o2 : bs) = min (pathLengths as bs) (pathLengths bs as)
  where pathLengths as bs
          = let first = find (\ a -> a `elem` bs) as in
              case first of
                  Nothing -> 0
                  Just point -> (indexOf as point) + (indexOf bs point) + 2

advance :: Move -> Point -> [Point]
advance move (x, y)
  = case move of
        MoveRight d -> reverse [(z, y) | z <- [(x + 1) .. (x + d)]]
        MoveUp d -> reverse [(x, z) | z <- [(y + 1) .. (y + d)]]
        MoveLeft d -> [(z, y) | z <- [(x - d) .. (x - 1)]]
        MoveDown d -> [(x, z) | z <- [(y - d) .. (y - 1)]]

tracePath :: [Move] -> [Point]
tracePath moves = reverse $ f moves [(0, 0)]
  where f :: [Move] -> [Point] -> [Point]
        f moves visited
          | length moves == 0 = visited
          | otherwise =
            f (tail moves) ((advance (head moves) (head visited)) ++ visited)

parseMove :: String -> Move
parseMove (m : ms)
  = case m of
        'R' -> MoveRight (read ms)
        'U' -> MoveUp (read ms)
        'L' -> MoveLeft (read ms)
        'D' -> MoveDown (read ms)

parseInput :: String -> ([Move], [Move])
parseInput input
  = let [l1, l2] = map ((map parseMove) . (splitOn ",")) (lines input) in
      (l1, l2)

part1 :: String -> Int
part1 input
  = let (ma, mb) = parseInput input
        as = tracePath ma
        bs = tracePath mb
      in closestCommon as bs

part2 :: String -> Int
part2 input
  = let (ma, mb) = parseInput input
        as = tracePath ma
        bs = tracePath mb
      in shortestCommon as bs

main = do
  input <- getContents
  print $ "Part 1: " ++ (show $ part1 input)
  print $ "Part 2: " ++ (show $ part2 input)
