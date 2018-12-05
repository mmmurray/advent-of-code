import qualified Data.List.Split as Split
import qualified Data.Text as Text
import Data.List

lowerChar :: Char -> Char
lowerChar c = Text.head . Text.toLower . Text.singleton $ c

doesReact :: String -> Bool
doesReact [x,y] = lowerChar x == lowerChar y && x /= y

removeReactionPairs :: String -> String
removeReactionPairs s = concat $ filter (\x -> length x /= 2 || (not $ doesReact x)) (Split.chunksOf 2 s)

removeReactions :: String -> String
removeReactions units =
  let
    withoutEvenUnits = removeReactionPairs units
    withoutOddUnits = removeReactionPairs $ tail withoutEvenUnits
    result = (Prelude.head withoutEvenUnits) : withoutOddUnits
  in if result == units then units else removeReactions result

removeUnit :: Char -> String -> String
removeUnit unit s = filter (\c -> lowerChar c /= unit) s

reactedLengthsWithoutUnit :: Char -> String -> Int
reactedLengthsWithoutUnit unit s = length . removeReactions . (removeUnit unit) $ s

main = do
  contents <- getContents
  let
    allUnits = ['a'..'z']
    lengths = map (\unit -> (unit, reactedLengthsWithoutUnit unit contents)) allUnits
    mostReactions = minimumBy (\(u1, l1) (u2, l2) -> compare l1 l2) lengths
  print $ snd mostReactions
