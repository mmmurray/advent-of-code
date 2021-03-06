import Text.Regex.PCRE
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


fastForward :: [Point] -> Int -> ([Point], Int)
fastForward points time
  | isMessage points = (points, time)
  | otherwise = fastForward (tick points) (time + 1)


main = do
  contents <- getContents
  let
    points = map parseLine (lines contents)
    (result, time) = fastForward points 0
    positions = map (\(d, v) -> d) result
  print positions
  print time

-- Plot with: http://www.shodor.org/interactivate/activities/SimplePlot/
-- Flip vertically and scale Y 33% to read message.
