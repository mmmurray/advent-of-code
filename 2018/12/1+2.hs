import Debug.Trace
import Text.Regex.PCRE
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Rules = Map.Map (Bool, Bool, Bool, Bool, Bool) Bool
type Pot = Int
type Pots = Set.Set Pot

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
    initialState = Set.fromList $ map fst potsWithPlant
    rules = Map.fromList (map parseRule (tail ruleLines))
  in
    (initialState, rules)


nextPotState :: Rules -> Pots -> Pot -> Bool
nextPotState rules pots pot =
  let
    l2 = (pot - 2) `Set.member` pots
    l1 = (pot - 1) `Set.member` pots
    c = pot `Set.member` pots
    r1 = (pot + 1) `Set.member` pots
    r2 = (pot + 2) `Set.member` pots
  in
    case Map.lookup (l2, l1, c, r1, r2) rules of
      Just state -> state
      Nothing -> False


evolve :: Rules -> Pots -> Int -> Pots
evolve rules pots generations =
  let
    min = Set.findMin pots - 2
    max = Set.findMax pots + 2
    newPots = List.foldr f pots [min..max]
      where
        f pot acc = if nextPotState rules pots pot
          then Set.insert pot acc
          else Set.delete pot acc
  in if generations == 1
    then newPots
    else evolve rules newPots (generations - 1)


main = do
  contents <- getContents
  let
    (initialState, rules) = parseInput contents
  print $ "Part 1: " ++ (show $ sum (evolve rules initialState 20))
  print $ "Part 2: " ++ (show $ sum (evolve rules initialState 50000000))
