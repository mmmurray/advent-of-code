import Text.Regex.PCRE
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type Rules = Map.Map (Bool, Bool, Bool, Bool, Bool) Bool
type Pot = Int
type Pots = [Pot]

parseRule :: String -> ((Bool, Bool, Bool, Bool, Bool), Bool)
parseRule rule =
  let
    matches = rule =~ "^(\\.|#)(\\.|#)(\\.|#)(\\.|#)(\\.|#) => (\\.|#)$" :: [[String]]
    [l2, l1, a, r1, r2, b] = map (== "#") $ tail . head $ matches
  in
    ((l2, l1, a, r1, r2), b)


parseInput :: String -> (Pots, Rules)
parseInput input =
  let
    (initialStateLine:ruleLines) = lines input
    pots = zip [0..] (drop (length "initial state: ") initialStateLine)
    potsWithPlant = filter (\(i, c) -> c == '#') pots
    initialState = map fst potsWithPlant
    rules = Map.fromList (map parseRule (tail ruleLines))
  in
    (initialState, rules)


hasPlant :: Pots -> Pot -> Bool
hasPlant (p:ps) pot
  | p == pot = True
  | p > pot = False
  | length ps == 0 = False
  | otherwise = hasPlant ps pot


nextPotState :: Rules -> Pots -> Pot -> Bool
nextPotState rules pots pot =
  let
    l2 = hasPlant pots (pot - 2)
    l1 = hasPlant pots (pot - 1)
    c = hasPlant pots pot
    r1 = hasPlant pots (pot + 1)
    r2 = hasPlant pots (pot + 2)
  in
    case Map.lookup (l2, l1, c, r1, r2) rules of
      Just state -> state
      Nothing -> False


evolve :: Rules -> Pots -> Pots
evolve rules pots =
  let
    min = head pots - 2
    max = last pots + 2
  in List.filter (nextPotState rules pots) [min..max]


evolveGenerations :: Rules -> Pots -> Int -> Pots
evolveGenerations rules pots generations = List.foldl' (\acc g -> evolve rules acc) pots [1..generations]


main = do
  contents <- getContents
  let
    (initialState, rules) = parseInput contents
  print $ "Part 1: " ++ (show $ sum (evolveGenerations rules initialState 20))
  print $ "Part 2: " ++ (show $ sum (evolveGenerations rules initialState 10000))
