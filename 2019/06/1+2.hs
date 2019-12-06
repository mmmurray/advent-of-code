import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.Split (splitOn)

type Orbit = String
type Orbits = Map Orbit Orbit

parseOrbits :: String -> Orbits
parseOrbits input = Map.fromList $ map parseOrbit $ lines input
  where
    parseOrbit input = let [a,b] = splitOn ")" input in (b,a)

traverseOrbits :: Orbits -> Orbit -> [(Orbit, Orbit)]
traverseOrbits = f []
  where
    f acc orbits "COM" = acc
    f acc orbits orbit = case Map.lookup orbit orbits of
      Just other -> f ((orbit, other) : acc) orbits other
      Nothing -> acc

part1 :: Orbits -> Int
part1 orbits = sum $ map (length . traverseOrbits orbits) $ Map.keys orbits

part2 :: Orbits -> Int
part2 orbits = let
    sanOrbits = traverseOrbits orbits "SAN"
    youOrbits = traverseOrbits orbits "YOU"
    sharedOrbitCount = 2 * (length $ List.intersect sanOrbits youOrbits) - 2
  in (length sanOrbits) + (length youOrbits) - sharedOrbitCount

main = do
  input <- getContents
  let orbits = parseOrbits input
  print $ "Part 1: " ++ (show $ part1 orbits)
  print $ "Part 2: " ++ (show $ part2 orbits)
