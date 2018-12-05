import Data.List.Split
import Data.Text (toLower, unpack, pack, head,singleton)
import Data.List

lowerChar :: Char -> Char
lowerChar c = Data.Text.head . toLower . singleton $ c

doesReact :: String -> Bool
doesReact [x,y] =
  lowerChar x == lowerChar y && x /= y

removeReactionPairs :: String -> String
removeReactionPairs s = concat $ filter (\x -> length x /= 2 || (not $ doesReact x)) (chunksOf 2 s)

removeReactions :: String -> String
removeReactions s1 =
  let
    s2 = removeReactionPairs s1
    s3 = removeReactionPairs $ tail s2
  in
    (Prelude.head s2) : s3

removeReactionsRec :: String -> String
removeReactionsRec s1 =
  let
    s2 = removeReactions s1
  in
    if s1 == s2 then s1 else removeReactionsRec s2

removeUnit :: Char -> String -> String
removeUnit unit s = filter (\c -> lowerChar c /= unit) s

reactedLengthsWithoutUnit :: Char -> String -> Int
reactedLengthsWithoutUnit unit s = length . removeReactionsRec . (removeUnit unit) $ s

main = do
  contents <- getContents
  let
    allUnits = ['a'..'z']
    lengths = map (\unit -> (unit, reactedLengthsWithoutUnit unit contents)) allUnits
    mostReactions = minimumBy (\(u1, l1) (u2, l2) -> compare l1 l2) lengths
  print $ snd mostReactions
