import Data.List.Split (chunksOf)
import Data.List (intercalate, minimumBy)
import Data.Ord (comparing)

parseInput :: String -> [Int]
parseInput "" = []
parseInput (d:ds) = (read [d]) : (parseInput ds)

countDigit :: Int -> [Int] -> Int
countDigit digit layer = length $ filter ((==) digit) layer

part1 :: [[Int]] -> Int
part1 layers =
  let
    c0 = countDigit 0
    layer0 = minimumBy (comparing c0) layers
  in (countDigit 1 layer0) * (countDigit 2 layer0)

resolveLayers :: [[Int]] -> [Int]
resolveLayers [] = []
resolveLayers [l] = l
resolveLayers (l1:ls) = zipWith overlay l1 (resolveLayers ls)
  where
    overlay 2 p = p
    overlay p _ = p

printImage :: Int -> [Int] -> String
printImage width pixels = intercalate "\n" $ map printLine (chunksOf width pixels)
  where
    printPixel 1 = '#'
    printPixel _ = ' '
    printLine [] = ""
    printLine (p:ps) = printPixel p : printLine ps

part2 :: [[Int]] -> [Int]
part2 layers = resolveLayers layers

main = do
  input <- getContents
  let layers = chunksOf (25 * 6) $ parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 layers)
  putStrLn $ "Part 2: "
  putStrLn $ printImage 25 $ part2 layers
