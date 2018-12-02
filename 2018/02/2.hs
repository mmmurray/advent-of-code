import Data.List

commonChars (a, b) = foldr f "" (zip a b)
  where f (l, r) acc = if l == r then l : acc else acc

main = do
  contents <- getContents
  let
    allPairs = [(x, y) | (x:ys) <- (tails . lines) contents, y <- ys]
    withOneDifference = filter (((==) 25) . length) $ map commonChars allPairs
  print withOneDifference
