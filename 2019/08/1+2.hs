import Data.List.Split (chunksOf)
import Data.List (intercalate, sortBy)

parseInput :: String -> [Int]
parseInput "" = []
parseInput (d:ds) = (read [d]) : (parseInput ds)

countDigit :: Int -> [Int] -> Int
countDigit digit layer = length $ filter ((==) digit) layer

part1 :: [[Int]] -> Int
part1 layers =
  let
    c0 = countDigit 0
    layer0 = head $ sortBy (\a b -> compare (c0 a) (c0 b)) layers
  in (countDigit 1 layer0) * (countDigit 2 layer0)

resolveLayers :: [[Int]] -> [Int]
resolveLayers [] = []
resolveLayers [l] = l
resolveLayers (l1:ls) = zipWith (\p1 p2 -> if p1 == 2 then p2 else p1) l1 (resolveLayers ls)

printImage :: Int -> [Int] -> String
printImage width pixels = intercalate "\n" $ map printLine (chunksOf width pixels)
  where
    printLine [] = ""
    printLine (p:ps) = (if p == 1 then '#' else ' ') : (printLine ps)

part2 :: [[Int]] -> [Int]
part2 layers = resolveLayers layers

main = do
  input <- getContents
  let layers = chunksOf (25 * 6) $ parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 layers)
  putStrLn $ "Part 2: "
  putStrLn $ printImage 25 $ part2 layers
