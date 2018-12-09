import Debug.Trace
import qualified Data.List as List
import qualified Data.Sequence as Sequence

type Player = Int
type Marble = Int
type Marbles = Sequence.Seq Marble

insertMarbleAfter :: Marble -> Marble -> Marbles -> Marbles
insertMarbleAfter marble after marbles =
  case Sequence.elemIndexL after marbles of
    Nothing -> marbles
    Just index ->
      let
        insertIndex = (index + 2) `mod` (length marbles)
      in
        Sequence.insertAt insertIndex marble marbles


getIndexOf :: Int -> Marbles -> Int
getIndexOf after marbles =
  case Sequence.elemIndexL after marbles of
    Nothing -> 0
    Just index -> index


playMarble :: Marble -> Marble -> Marbles -> (Int, Marble, Marbles)
playMarble marble after marbles
  | marble `mod` 23 == 0 =
    let
      index = getIndexOf after marbles
      removeIndex = (index - 7) `mod` (length marbles)
      currentIndex = (index - 6) `mod` (length marbles)
      newCurrent = Sequence.index marbles currentIndex
      removed = Sequence.index marbles removeIndex
      newMarbles = Sequence.deleteAt removeIndex marbles
    in (marble + removed, newCurrent, newMarbles)
  | otherwise = (0, marble, insertMarbleAfter marble after marbles)


haveTurns :: Int -> Marble -> Marble -> Marbles -> [Player] -> [Player]
haveTurns turnsLeft nextMarble currentMarble marbles (currentPlayer:otherPlayers)
  | turnsLeft == 0 = currentPlayer : otherPlayers
  | otherwise =
    let
      (score, newCurrent, newMarbles) = playMarble nextMarble currentMarble marbles
      nextPlayers = otherPlayers ++ [currentPlayer + score]
    in haveTurns (turnsLeft - 1) (nextMarble + 1) newCurrent newMarbles nextPlayers


main = do
  let
    numberOfPlayers = 10
    highestMarble = 1618

    -- numberOfPlayers = 405
    -- highestMarble = 7095300
    players = take numberOfPlayers (repeat 0)
    scores = haveTurns highestMarble 1 0 (Sequence.fromList [0]) players
  print $ maximum scores
