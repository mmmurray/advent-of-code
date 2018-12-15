import qualified Data.Set as Set

type Point = (Int, Int)
type Player = (Point, Int, Bool)
type Walls = Set.Set Point

initialHP = 200

parseInput :: String -> [(Point, Char)]
parseInput input = filter (\(v, c) -> c /= '.') $ concatMap parseLine (zip [0..] (lines input))
  where parseLine (y, line) = map (\(x, c) -> ((x, y), c)) (zip [0..] line)


loadMap :: String -> ([Point], [Player])
loadMap input = foldl f ([], []) (parseInput input)
  where
    f (walls, players) (position, char) =
      let
        (newWalls, newPlayers) = case char of
          '#' -> ([position], [])
          'E' -> ([], [(position, initialHP, False)])
          'G' -> ([], [(position, initialHP, True)])
      in
        (walls ++ newWalls, players ++ newPlayers)


main = do
  contents <- getContents
  print $ loadMap contents
