import Data.List.Split

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs) ++ x : (drop (i + 1) xs)

executeOperation :: Int -> (Int -> Int -> Int) -> [Int] -> [Int]
executeOperation ip operation memory =
  let a = memory !! (memory !! (ip + 1))
      b = memory !! (memory !! (ip + 2))
      i = memory !! (ip + 3)
      r = operation a b
  in  setElement i r memory

execute :: Int -> [Int] -> [Int]
execute ip memory = case memory !! ip of
  99 -> memory
  1  -> execute (ip + 4) $ executeOperation ip (+) memory
  2  -> execute (ip + 4) $ executeOperation ip (*) memory

run :: Int -> Int -> String -> Int
run n v input = head $ execute 0 $ initMemory n v $ parseInput input
  where
    initMemory a b memory = setElement 1 a $ setElement 2 b memory
    parseInput input = map read $ splitOn "," input

part1 :: String -> Int
part1 = run 12 2

part2 :: String -> Int
part2 input =
  let (n, v) =
          head
            [ (n, v)
            | n <- [0 .. 100]
            , v <- [0 .. 100]
            , run n v input == 19690720
            ]
  in  n * 100 + v

main = do
  input <- getContents
  print $ "Part 1: " ++ (show $ part1 input)
  print $ "Part 2: " ++ (show $ part2 input)
