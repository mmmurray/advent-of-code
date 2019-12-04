import Data.Char
import Data.List

main = do
  let input = map (map digitToInt . show) [134792..675810]
  let ascending = filter (\d -> sort d == d) input
  print $ "Part 1: " ++ (show $ length $ filter hasGroup ascending)
  print $ "Part 2: " ++ (show $ length $ filter hasGroupOf2 ascending)
  where
    hasGroup d = (length $ groupBy (==) d) < length d
    hasGroupOf2 d = elem 2 $ map length $ groupBy (==) d
