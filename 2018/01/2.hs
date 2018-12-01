import qualified Data.List as List
import qualified Data.Set as Set

lineToNumber :: String -> Int
lineToNumber (sign:digits) =
  let magnitude = read digits
  in if sign == '+'
    then magnitude
    else -magnitude

parseFequencies :: String -> [Int]
parseFequencies input =
  List.map lineToNumber (lines input)

getFirstDuplicate :: (Set.Set Int) -> [Int] -> Int
getFirstDuplicate seen (x:xs) =
  if Set.member x seen
    then x
    else getFirstDuplicate (Set.insert x seen) xs

run :: String -> Int
run input =
  let
    frequencies = cycle $ parseFequencies input
    cumulativeFrequencies = scanl (+) 0 frequencies
    firstDuplicate = getFirstDuplicate Set.empty cumulativeFrequencies
  in
    firstDuplicate

main = do
  contents <- getContents
  print $ run contents
