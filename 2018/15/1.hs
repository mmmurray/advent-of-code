import Debug.Trace
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int, Int)
type Player = (Point, Int, Bool) -- position, HP, is goblin
type Walls = Set.Set Point

initialHP = 200
attackDamage = 3
noPoint = (-1, -1)


adjcentPoints :: Point -> [Point]
adjcentPoints (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]


dijkstra :: Set.Set Point -> Map.Map Point Int -> [(Point, Int)] -> Map.Map Point Int
dijkstra spaces found queue
  -- | trace (show found) False = undefined
  | length queue == 0 = found
  | otherwise =
      let
        (qPoint, distance) = head queue
        newDistance = distance + 1
        adjcentSpaces = filter (\p -> Set.member p spaces) (adjcentPoints qPoint)
        closer = filter (\p -> (not $ Map.member p found) || newDistance < (found Map.! p)) adjcentSpaces
        newFound = foldl (\acc p -> Map.insert p newDistance acc) found closer
      in dijkstra spaces newFound ((tail queue) ++ (map (\p -> (p, newDistance)) closer))


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


sortPlayers :: [Player] -> [Player]
sortPlayers = List.sortBy (\(p1, _, _) (p2, _, _) -> readingOrder p1 p2)


sortWeakest :: [Player] -> [Player]
sortWeakest = List.sortBy (\(_, hp1, _) (_, hp2, _) -> compare hp1 hp2)


readingOrder :: Point -> Point -> Ordering
readingOrder (x1, y1) (x2, y2)
  | y1 < y2 = LT
  | y1 > y2 = GT
  | x1 < x2 = LT
  | x1 > x2 = GT
  | otherwise = EQ


isAdjacent :: Point -> Point -> Bool
isAdjacent (x1, y1) (x2, y2)
  | x1 == x2 && abs (y2 - y1) == 1 = True
  | y1 == y2 && abs (x2 - x1) == 1 = True
  | otherwise = False


enemiesInRange :: [Player] -> Player -> [Player]
enemiesInRange players (p1, _, t1) = filter (\(p2, _, t2) -> t1 /= t2 && isAdjacent p1 p2) players


attackEnemyInRange :: [Player] -> Player -> (Bool, [Player])
attackEnemyInRange players player =
  let
    targets = sortWeakest $ sortPlayers $ enemiesInRange players player
    attackerPosition = if length targets > 0
      then (let (p2, _, _) = head targets in p2)
      else noPoint

    damagedPlayers = map (\(p2, hp, t2) -> if attackerPosition == p2 then (p2, hp - attackDamage, t2) else (p2, hp, t2)) players

    newPlayers = filter (\(_, hp, _) -> hp > 0) damagedPlayers
  in
    (attackerPosition /= noPoint, newPlayers)


movePlayer :: Walls -> [Player] -> Player -> (Player, [Player])
movePlayer walls players (p1, hp1, t1) =
  let
    (xMax, yMax) = Set.findMax walls
    playerPoints = Set.fromList $ map (\(p, _, _) -> p) players
    allPoints = Set.fromList [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    spaces = allPoints Set.\\ walls Set.\\ playerPoints

    enemies = filter (\(_, _, t2) -> t1 /= t2) players
    adjcent = adjcentPoints p1

    f (p2, _, _) =
      let
        distances = dijkstra spaces (Map.fromList [(p2, 0)]) [(p2, 0)]
        possible = filter (\p -> Map.member p distances && Set.member p spaces) adjcent
      in
        map (\p -> (p, distances Map.! p)) possible

    candidates = concatMap f enemies
    readingOrderCandidates = List.sortBy (\(p1, d1) (p2, d2) -> readingOrder p1 p2) candidates
    bestCandidates = List.sortBy (\(p1, d1) (p2, d2) -> compare d1 d2) readingOrderCandidates
    moveTo = if length bestCandidates > 0 then (fst . head) bestCandidates else noPoint
    newPlayer = if moveTo == noPoint then (p1, hp1, t1) else (moveTo, hp1, t1)
    newPlayers = map (\(p2, hp, t) -> if p2 == p1 then newPlayer else (p2, hp, t)) players
  in
    (newPlayer, newPlayers)


takeTurn :: Walls -> [Player] -> Player -> [Player]
takeTurn walls players player
  -- | trace ("Player turn: " ++ (show player)) False = undefined
  -- | trace ("Player turn: " ++ (show player) ++ "\n" ++ (printPoints $ renderState walls players)) False = undefined
  | otherwise =
  let
    shouldMove = length (enemiesInRange players player) == 0
    (movedPlayer, movedPlayers) = if shouldMove then movePlayer walls players player else (player, players)
    (didAttack, attackedPlayers) = attackEnemyInRange movedPlayers movedPlayer
  in
    attackedPlayers


takeTurns :: Walls -> [Player] -> [Player]
takeTurns walls players =
  let
    orderedPlayers = sortPlayers players
  in
    List.foldl' (\acc player -> takeTurn walls acc player) orderedPlayers orderedPlayers
    -- List.foldl' (\acc player -> if List.elem player acc then takeTurn walls acc player else acc) orderedPlayers orderedPlayers


battleComplete :: [Player] -> Bool
battleComplete players =
  let
    elves = (map (\(p, _, _) -> p) (filter (\(_, _, t) -> not t) players))
    goblins = (map (\(p, _, _) -> p) (filter (\(_, _, t) -> t) players))
  in
    (length elves == 0) || (length goblins == 0)


totalHP :: [Player] -> Int
totalHP players = sum (map (\(_, hp, _) -> hp) players)


battle :: Walls -> [Player] -> Int -> Int
battle walls players turn
  | trace ((show turn) ++ (":\n") ++ (printPlayers players) ++ ("\n") ++ (printPoints $ renderState walls players)) False = undefined
  -- | turn == 30 = 0
  | battleComplete players = turn * (totalHP players)
  | otherwise = battle walls (takeTurns walls players) (turn + 1)


printPlayers :: [Player] -> String
printPlayers players =
  let
    parts = map (\(p, hp, t) -> (if t then "G" else "E") ++ "(" ++ (show hp) ++ ")" ++ (show p)) (sortPlayers players)
  in
    List.intercalate ", " parts


printPoints :: Map.Map Point Char -> String
printPoints points =
  let
    (maxP, _) = Map.findMax points
    (xMax, yMax) = maxP
  in unlines $ map (l xMax) [0..yMax]
    where l xMax y = map (\x -> if Map.member (x,y) points then points Map.! (x,y) else '.') [0..xMax]


renderStateWithDistance :: Walls -> [Player] -> Map.Map Point Int -> Map.Map Point Char
renderStateWithDistance walls players distances =
  let
    (xMax, yMax) = Set.findMax walls
    allPoints = [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    elves = Set.fromList (map (\(p, _, _) -> p) (filter (\(_, _, t) -> not t) players))
    goblins = Set.fromList (map (\(p, _, _) -> p) (filter (\(_, _, t) -> t) players))
  in Map.fromList (map (\p -> (p, f p elves goblins)) allPoints)
      where f p elves goblins
              | Map.member p distances = head $ show $ distances Map.! p
              | Set.member p walls = '#'
              | Set.member p elves = 'E'
              | Set.member p goblins = 'G'
              | otherwise = '.'

renderState :: Walls -> [Player] -> Map.Map Point Char
renderState walls players = renderStateWithDistance walls players (Map.fromList [])

main = do
  contents <- getContents
  let
    (wallPoints, players) = loadMap contents
    (xMax, yMax) = maximum wallPoints
    allPointsList = [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    allPoints = Set.fromList allPointsList
    walls = Set.fromList wallPoints
    playerPoints = Set.fromList $ map (\(p, _, _) -> p) players
    spaces = allPoints Set.\\ walls
    -- distances = dijkstra spaces (Map.fromList [((1, 1), 0)]) [((1, 1), 0)]

    result = battle walls players 0


  -- print $ players
  -- print $ players2
  -- print $ isAdjacent (1, 2) (1, 3)
  -- print $ takeTurn walls players
  -- print $ spaces
  -- print $ distances
  -- putStr $ printPoints $ renderState walls players
  -- putStr $ printPoints $ renderState walls players2
  print result
