import qualified Data.List as List
import qualified Data.Map as Map

data TrackType = Straight | Intersection | CornerA | CornerB deriving (Show)
type Vector = (Int, Int)
type Cart = (Vector, Vector, Int) -- position, direction, last intersection
type TrackSection = (Vector, TrackType)
type Track = Map.Map Vector TrackType


vAdd :: Vector -> Vector -> Vector
vAdd (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)


vTurn :: Int -> Vector -> Vector
vTurn turn (dx, dy) = (-dy * turn, dx * turn)


parseChar :: Char -> ([TrackType], [Vector])
parseChar c
  | c == '-' || c == '|' = ([Straight], [])
  | c == '+' = ([Intersection], [])
  | c == '/' = ([CornerA], [])
  | c == '\\' = ([CornerB], [])
  | c == '^' = ([Straight], [(0, -1)])
  | c == '>' = ([Straight], [(1, 0)])
  | c == 'v' = ([Straight], [(0, 1)])
  | c == '<' = ([Straight], [(-1, 0)])


parseInput :: String -> [(Vector, Char)]
parseInput input = filter (\(v, c) -> c /= ' ') $ concatMap parseLine (zip [0..] (lines input))
  where parseLine (y, line) = map (\(x, c) -> ((x, y), c)) (zip [0..] line)


loadTrack :: [(Vector, Char)] -> ([TrackSection], [Cart])
loadTrack input = foldl f ([], []) input
  where
    f (tracks, carts) (position, char) =
      let
        (trackTypes, cartDirections) = parseChar char
        newTracks = map (\trackType -> (position, trackType)) trackTypes
        newCarts = map (\direction -> (position, direction, -1)) cartDirections
      in
        (tracks ++ newTracks, carts ++ newCarts)


turnCorner :: Int -> Vector -> Vector
turnCorner corner (dx, dy) = (dy * (-corner), dx * (-corner))


turnIntersection :: Int -> Vector -> (Vector, Int)
turnIntersection decision direction
  | decision == 0 = (direction, 1)
  | decision == 1 = (vTurn decision direction, -1)
  | decision == -1 = (vTurn decision direction, 0)


moveCart :: Track -> Cart -> Cart
moveCart track (position, direction, nextDecision) =
  let
    trackType = (Map.!) track position
    (newDirection, newNextDecision) =
      case trackType of
        Straight -> (direction, nextDecision)
        Intersection -> turnIntersection nextDecision direction
        CornerA -> (turnCorner 1 direction, nextDecision)
        CornerB -> (turnCorner (-1) direction, nextDecision)
  in
    (vAdd position newDirection, newDirection, newNextDecision)


sortCarts :: [Cart] -> [Cart]
sortCarts = List.sortBy (\(p1, _, _) (p2, _, _) -> sorter p1 p2)


sorter :: Vector -> Vector -> Ordering
sorter (x1, y1) (x2, y2)
  | x1 < x2 = LT
  | x1 > x2 = GT
  | y1 < y2 = LT
  | y1 > y2 = GT
  | otherwise = EQ


cartCollides :: [Cart] -> Cart -> Bool
cartCollides carts (position, _, _) =
  let
    positions = map (\(p, _, _) -> p) carts
    colliding = filter (== position) positions
  in
    length colliding > 0


removeCollisions :: [Cart] -> [Cart]
removeCollisions carts =
  let
    positions = map (\(p, _, _) -> p) carts
    grouped = List.group $ List.sort positions
    collisions = map head (filter ((== 2) . length) grouped)
  in
    filter (\(p, _, _) -> not $ List.elem p collisions) carts


tick :: Track -> [Cart] -> [Cart] -> ([Cart], [Vector])
tick track movedCarts (c:cs) =
  let
    newCart = moveCart track c
    (newPosition, _, _) = newCart
    collides = cartCollides (movedCarts ++ cs) newCart
    collisions = if collides then [newPosition] else []
  in
    if length cs == 0
      then (newCart : movedCarts, collisions)
      else
        let
          (nextCarts, nextCollisions) = tick track (newCart : movedCarts) cs
        in (nextCarts, collisions ++ nextCollisions)


tickUntilCrash :: Track -> [Cart] -> Vector
tickUntilCrash track carts =
  let
    (newCarts, collisions) = tick track [] carts
  in if length collisions > 0 then head collisions else tickUntilCrash track (sortCarts newCarts)


tickAndRemove :: Track -> [Cart] -> [Cart] -> [Cart]
tickAndRemove track movedCarts (c:cs) =
  let
    newCart = moveCart track c
    collidesWithMoved = cartCollides movedCarts newCart
    collidesWithNext = cartCollides cs newCart
    nextMovedCarts = if collidesWithNext then movedCarts else removeCollisions $ movedCarts ++ [newCart]
    nextCarts = if collidesWithNext then removeCollisions (newCart : cs) else cs
    in if length nextCarts == 0 then nextMovedCarts else tickAndRemove track nextMovedCarts nextCarts


lastCartStanding :: Track -> [Cart] -> Cart
lastCartStanding track carts =
  let
    newCarts = tickAndRemove track [] (sortCarts carts)
  in if length newCarts == 1 then head newCarts else lastCartStanding track newCarts


main = do
  contents <- getContents
  let
    input = parseInput contents
    (trackSections, carts) = loadTrack input
    track = Map.fromList trackSections
  print $ "Part 1: " ++ (show $ tickUntilCrash track (sortCarts carts))
  print $ "Part 2: " ++ (show $ lastCartStanding track carts)
