import Data.List

countGroups groups = (min 1 (withLength 2), min 1 (withLength 3))
  where withLength l = length (filter (((==) l) . length) groups)

summedGroups groups = foldl f (0, 0) groups
  where f (x, y) (a, b) = (x + a, y + b)

main = do
  contents <- getContents
  let
    (x, y) = summedGroups . map (countGroups . group . sort) $ lines contents
  print $ x * y
