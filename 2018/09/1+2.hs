import qualified Data.List as List
import qualified Data.Sequence as Sequence

type Player = Int
type Marble = Int
type Marbles = Sequence.Seq Marble


playMarble :: Marble -> Marble -> Marbles -> (Int, Marble, Marbles)
playMarble marble currentIndex marbles
  | marble `mod` 23 == 0 =
    let
      removeIndex = (currentIndex - 7) `mod` (length marbles)
      removed = Sequence.index marbles removeIndex
      newMarbles = Sequence.deleteAt removeIndex marbles
    in (marble + removed, removeIndex, newMarbles)
  | otherwise =
    let
      insertIndex = (currentIndex + 2) `mod` (length marbles)
    in (0, insertIndex, Sequence.insertAt insertIndex marble marbles)


haveTurns :: Int -> Marble -> Int -> Marbles -> [Player] -> [Player]
haveTurns turnsLeft nextMarble currentIndex marbles (currentPlayer:otherPlayers)
  | turnsLeft == 0 = currentPlayer : otherPlayers
  | otherwise =
    let
      (score, newCurrentIndex, newMarbles) = playMarble nextMarble currentIndex marbles
      nextPlayers = otherPlayers ++ [currentPlayer + score]
    in haveTurns (turnsLeft - 1) (nextMarble + 1) newCurrentIndex newMarbles nextPlayers


playGame :: Int -> Int -> Int
playGame numberOfPlayers highestMarble =
  let
    players = take numberOfPlayers (repeat 0)
    scores = haveTurns highestMarble 1 0 (Sequence.fromList [0]) players
  in
    maximum scores


main = do
  print $ "Part 1 " ++ (show $ playGame 405 70953)
  print $ "Part 2 " ++ (show $ playGame 405 7095300)
