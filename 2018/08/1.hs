import qualified Data.List.Split as Split
import Debug.Trace

data Tree = Empty | Node [Tree] [Int] deriving (Show)

parseInput :: String -> [Int]
parseInput input = map read $ Split.splitOn " " input


foldFn :: ([Tree], [Int]) -> Int -> ([Tree], [Int])
foldFn (children, n1) childIndex =
  let
    (n2, child) = generateTree n1
  in
    (child : children, n2)


getChildren :: Int -> [Int] -> ([Tree], [Int])
getChildren childrenCount n1
  | childrenCount == 0 = ([], n1)
  | otherwise = foldl foldFn ([], n1) (tail [0..childrenCount])


generateTree :: [Int] -> ([Int], Tree)
generateTree (childrenCount:metadataCount:n1) =
  let
    (children, n2) = getChildren childrenCount n1
    (metadata, n3) = splitAt metadataCount n2
  in (n3, Node children metadata)


sumMetadata :: Tree -> Int
sumMetadata tree =
  case tree of
    Empty -> 0
    Node children metadata -> (sum metadata) + (sum $ map sumMetadata children)


main = do
  contents <- getContents
  let
    n = parseInput contents
    (_, t) = generateTree n
  print $ sumMetadata t
