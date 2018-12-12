import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Rules = Map.Map (Bool, Bool, Bool, Bool, Bool, Bool) Bool
type Pot = Int
type Pots = Set.Set Pot

parseInput :: String -> (Pots, Rules)
parseInput input =
  let
    (initialStateLine:ruleLines) = lines input

    rules = Map.fromList []
    initialState = Set.fromList []
  in
    (initialState, rules)

evolve :: Rules -> Pots -> Pots
evolve rules pots = pots

main = do
  contents <- getContents
  let
    (initialState, rules) = parseInput contents
  print $ initialState
  print $ rules
  