import Text.Regex.PCRE
import qualified Data.Set as Set
import qualified Data.List as List

type Vector = (Int, Int)
type Point = (Vector, Vector)

parseLine :: String -> Point
parseLine line =
  let
    matches = line =~ "position=<\\s*(-?\\d+), \\s*(-?\\d+)> velocity=<\\s*(-?\\d+), \\s*(-?\\d+)>" :: [[String]]
    [dx, dy, vx, vy] = map read $ tail . head $ matches
  in
    ((dx, dy), (vx, vy))

tick :: [Point] -> [Point]
tick = map (\(d, v) ->
  let
    (dx, dy) = d
    (vx, vy) = v
  in ((dx + vx, dy + vy), (vx, vy)))

isMessage :: [Point] -> Bool
isMessage points =
  let
    yPositions = map ((\(dx, dy) -> dy) . (\(d, v) -> d)) points
    range = (maximum yPositions) - (minimum yPositions)
  in
    range < 10


moveToOrigin :: [Point] -> [Vector]
moveToOrigin points =
  let
    positions = map (\(d, v) -> d) points
    minY = minimum $ map (\(dx, dy) -> dy) positions
    minX = minimum $ map (\(dx, dy) -> dx) positions
  in
    map (\(dx, dy) -> (dx - minX, dy - minY)) positions


fastForward :: [Point] -> [Point]
fastForward points
  | isMessage points = points
  | otherwise = fastForward $ tick points


main = do
  contents <- getContents
  let
    points = map parseLine (lines contents)
    result = moveToOrigin $ fastForward points
  print result


-- tbSKMSe
-- http://www.shodor.org/interactivate/activities/SimplePlot/
