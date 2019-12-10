import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Ord (comparing)

type Point = (Int, Int)
type Angle = (Int, Int)

parseInput :: String -> [Point]
parseInput input = parseLines 0 $ lines input
  where
    parseLines :: Int -> [String] -> [Point]
    parseLines y [] = []
    parseLines y (line:lines) = (parseLine y line) ++ (parseLines (y + 1) lines)

    parseLine :: Int -> String -> [Point]
    parseLine y line = map fst $ filter ((== '#') . snd) $ zip [(x, y) | x <- [0..]] line

angleToRadians :: Angle -> Double
angleToRadians (x,y) = let
    r = atan2 (fromIntegral x) (fromIntegral y)
  in if r < 0 then pi + pi + r else r

compareAngles :: Angle -> Angle -> Ordering
compareAngles a1 a2 = compare (angleToRadians a1) (angleToRadians a2)

angleBetween :: Point -> Point -> Angle
angleBetween (x1,y1) (x2,y2) = let
    dx = x2 - x1
    dy = y1 - y2
    scale = gcd dx dy
  in (dx `div` scale, dy `div` scale)

distanceBetween :: Point -> Point -> Int
distanceBetween (x1,y1) (x2,y2) = (abs (x2 - x1)) + (abs (y2 - y1))

lineOfSightMap :: [Point] -> Point -> Map Angle (Int, Point)
lineOfSightMap points point = f point points mempty
  where
    f :: Point -> [Point] -> Map Angle (Int, Point) -> Map Angle (Int, Point)
    f p1 [] los = los
    f p1 (p2:ps) los = if p1 == p2 then f p1 ps los else
      let
        a = angleBetween p1 p2
        d1 = distanceBetween p1 p2
      in case Map.lookup a los of
        Just (d2, p3) -> f p1 ps $ Map.insert a (if d1 < d2 then (d1, p2) else (d2, p3)) los
        Nothing -> f p1 ps $ Map.insert a (d1, p2) los

vaporize :: [Point] -> Point -> [Point]
vaporize [] p = []
vaporize asteroids p =
  let
    los = lineOfSightMap asteroids p
    angles = List.sortBy compareAngles (Map.keys los)
    points = map (snd . ((Map.!) los)) angles
    remainingPoints = asteroids List.\\ (p:points)
  in points ++ (vaporize remainingPoints p)

countVisibleAsteroids :: [Point] -> Point -> Int
countVisibleAsteroids asteroids p = length . Map.keys $ lineOfSightMap asteroids p

bestStation :: [Point] -> Point
bestStation asteroids = List.maximumBy (comparing $ countVisibleAsteroids asteroids) asteroids

main = do
  input <- getContents
  let asteroids = parseInput input
  print $ "Part 1: " ++ (show $ countVisibleAsteroids asteroids $ bestStation asteroids)
  print $ "Part 2: " ++ (show $ (vaporize asteroids $ bestStation asteroids) !! 199)
