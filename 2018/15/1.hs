import Debug.Trace
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int, Int)
type Player = (Int, Point, Int, Bool) -- id, position, HP, is goblin
type Walls = Set.Set Point

initialHP = 200
elfAttackDamage = 19
goblinAttackDamage = 3
noPoint = (-1, -1)


adjcentPoints :: Point -> [Point]
adjcentPoints (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]


dijkstra :: Set.Set Point -> Map.Map Point Int -> [(Point, Int)] -> Map.Map Point Int
dijkstra spaces found queue
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
          'E' -> ([], [(0, position, initialHP, False)])
          'G' -> ([], [(0, position, initialHP, True)])
      in
        (walls ++ newWalls, players ++ newPlayers)


sortPlayers :: [Player] -> [Player]
sortPlayers = List.sortBy (\(_, p1, _, _) (_, p2, _, _) -> readingOrder p1 p2)


sortWeakest :: [Player] -> [Player]
sortWeakest = List.sortBy (\(_, _, hp1, _) (_, _, hp2, _) -> compare hp1 hp2)


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
enemiesInRange players (_, p1, _, t1) = filter (\(_, p2, _, t2) -> t1 /= t2 && isAdjacent p1 p2) players


attackEnemyInRange :: [Player] -> Player -> (Bool, [Player])
attackEnemyInRange players player =
  let
    targets = sortWeakest $ sortPlayers $ enemiesInRange players player
    attackerPosition = if length targets > 0
      then (let (_, p2, _, _) = head targets in p2)
      else noPoint

    (_, _, _, t1) = player
    attackDamage = if t1 then goblinAttackDamage else elfAttackDamage

    damagedPlayers = map (\(id2, p2, hp, t2) -> if attackerPosition == p2 then (id2, p2, hp - attackDamage, t2) else (id2, p2, hp, t2)) players

    newPlayers = filter (\(_, _, hp, _) -> hp > 0) damagedPlayers
  in
    (attackerPosition /= noPoint, newPlayers)


movePlayer :: Walls -> [Player] -> Player -> (Player, [Player])
movePlayer walls players (id1, p1, hp1, t1) =
  let
    (xMax, yMax) = Set.findMax walls
    playerPoints = Set.fromList $ map (\(_, p, _, _) -> p) players
    allPoints = Set.fromList [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    spaces = allPoints Set.\\ walls Set.\\ playerPoints
    enemies = filter (\(_, _, _, t2) -> t1 /= t2) players
    adjcent = adjcentPoints p1

    f (_, p2, _, _) =
      let
        distances = dijkstra spaces (Map.fromList [(p2, 0)]) [(p2, 0)]
        possible = filter (\p -> Map.member p distances && Set.member p spaces) adjcent
      in
        map (\p -> (p, distances Map.! p)) possible

    candidates = concatMap f enemies
    readingOrderCandidates = List.sortBy (\(p1, d1) (p2, d2) -> readingOrder p1 p2) candidates
    bestCandidates = List.sortBy (\(p1, d1) (p2, d2) -> compare d1 d2) readingOrderCandidates
    moveTo = if length bestCandidates > 0 then (fst . head) bestCandidates else noPoint
    newPlayer = if moveTo == noPoint then (id1, p1, hp1, t1) else (id1, moveTo, hp1, t1)
    newPlayers = map (\(id2, p2, hp, t) -> if p2 == p1 then newPlayer else (id2, p2, hp, t)) players
  in
    (newPlayer, newPlayers)


takeTurn :: Walls -> [Player] -> Player -> [Player]
takeTurn walls players player =
  let
    shouldMove = length (enemiesInRange players player) == 0
    (movedPlayer, movedPlayers) = if shouldMove then movePlayer walls players player else (player, players)
    (didAttack, attackedPlayers) = attackEnemyInRange movedPlayers movedPlayer
  in
    attackedPlayers


playerInList :: [Player] -> Player -> Bool
playerInList players (id1, _, _, _) = length (filter (\(id2, _, _, _) -> id1 == id2) players) == 1


takeTurns :: Walls -> [Player] -> [Player]
takeTurns walls players =
  let
    orderedPlayers = sortPlayers players
  in
    List.foldl' (\acc player -> if playerInList acc player then takeTurn walls acc player else acc) orderedPlayers orderedPlayers


battleComplete :: [Player] -> Bool
battleComplete players =
  let
    elves = (map (\(_, p, _, _) -> p) (filter (\(_, _, _, t) -> not t) players))
    goblins = (map (\(_, p, _, _) -> p) (filter (\(_, _, _, t) -> t) players))
  in
    (length elves == 0) || (length goblins == 0)


totalHP :: [Player] -> Int
totalHP players = sum (map (\(_, _, hp, _) -> hp) players)


battle :: Walls -> [Player] -> Int -> (Int, [Player])
battle walls players turn
  | trace ((show turn) ++ (":\n") ++ (printPlayers players) ++ ("\n") ++ (printPoints $ renderState walls players)) False = undefined
  | battleComplete players = (turn, players)
  | otherwise = battle walls (takeTurns walls players) (turn + 1)


printPlayers :: [Player] -> String
printPlayers players =
  let
    parts = map (\(id, p, hp, t) -> (if t then "G" else "E") ++ (show id) ++ "(" ++ (show hp) ++ ")" ++ (show p)) (sortPlayers players)
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
    elves = Set.fromList (map (\(_, p, _, _) -> p) (filter (\(_, _, _, t) -> not t) players))
    goblins = Set.fromList (map (\(_, p, _, _) -> p) (filter (\(_, _, _, t) -> t) players))
  in Map.fromList (map (\p -> (p, f p elves goblins)) allPoints)
      where f p elves goblins
              | Map.member p distances = head $ show $ distances Map.! p
              | Set.member p walls = '#'
              | Set.member p elves = 'E'
              | Set.member p goblins = 'G'
              | otherwise = '.'


renderState :: Walls -> [Player] -> Map.Map Point Char
renderState walls players = renderStateWithDistance walls players (Map.fromList [])


playerWithId :: Player -> Int -> Player
playerWithId (_, p, hp, t) id = (id, p, hp, t)


main = do
  contents <- getContents
  let
    (wallPoints, playersWithoutIds) = loadMap contents
    players = map (\(id, player) -> playerWithId player id) (zip [0..] playersWithoutIds)
    totalElves = length $ filter (\(_, _, _, t) -> not t) players
    (xMax, yMax) = maximum wallPoints
    allPointsList = [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    allPoints = Set.fromList allPointsList
    walls = Set.fromList wallPoints
    playerPoints = Set.fromList $ map (\(_, p, _, _) -> p) players
    spaces = allPoints Set.\\ walls
    (turn, victors) = battle walls players 0
    result = (turn - 1) * (totalHP victors)
  print $ (length victors) == totalElves
  putStr $ printPoints $ renderState walls victors
  print turn
  print $ totalHP victors
  print result
