import Debug.Trace
import qualified Data.List as List

type Player = Int
type Marble = Int

insertAt :: Int -> Marble-> [Marble] -> [Marble]
insertAt index marble marbles = front ++ (marble:back)
  where (front, back) = splitAt index marbles


removeAt :: Int-> [Marble] -> (Marble, [Marble])
removeAt index marbles =
  let
    (front, back) = splitAt index marbles
    (remove:keep) = back
  in
    (remove, front ++ keep)


insertMarbleAfter :: Marble -> Marble -> [Marble] -> [Marble]
insertMarbleAfter marble after marbles =
  case List.elemIndex after marbles of
    Nothing -> marbles
    Just index ->
      let
        insertIndex = (index + 2) `mod` (length marbles)
      in
        insertAt insertIndex marble marbles


getIndexOf :: Int -> [Marble] -> Int
getIndexOf after marbles =
  case List.elemIndex after marbles of
    Nothing -> 0
    Just index -> index


playMarble :: Marble -> Marble -> [Marble] -> (Int, Marble, [Marble])
playMarble marble after marbles
  | marble `mod` 23 == 0 =
    let
      index = getIndexOf after marbles
      removeIndex = (index - 7) `mod` (length marbles)
      currentIndex = (index - 6) `mod` (length marbles)
      newCurrent = marbles !! currentIndex
      (removed, newMarbles) = removeAt removeIndex marbles
    in (marble + removed, newCurrent, newMarbles)
  | otherwise = (0, marble, insertMarbleAfter marble after marbles)


haveTurns :: Int -> Marble -> Marble -> [Marble] -> [Player] -> [Player]
haveTurns turnsLeft nextMarble currentMarble marbles (currentPlayer:otherPlayers)
  | turnsLeft == 0 = currentPlayer : otherPlayers
  | otherwise =
    let
      (score, newCurrent, newMarbles) = playMarble nextMarble currentMarble marbles
      nextPlayers = otherPlayers ++ [currentPlayer + score]
    in haveTurns (turnsLeft - 1) (nextMarble + 1) newCurrent newMarbles nextPlayers


main = do
  let
    numberOfPlayers = 405
    highestMarble = 70953
    players = take numberOfPlayers (repeat 0)
    scores = haveTurns highestMarble 1 0 [0] players
  print $ maximum scores
