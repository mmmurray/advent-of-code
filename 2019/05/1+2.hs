import Data.List.Split
import Data.Char

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs) ++ x : (drop (i + 1) xs)

padLeft :: Int -> [Int] -> [Int]
padLeft l d = (replicate (l - length d) 0) ++ d

parseOp :: Int -> ([Int], Int)
parseOp op =
  let
    digits = map digitToInt $ show op
    modes = take ((length digits) - 2) digits
    opCodeDigits = padLeft 2 $ drop ((length digits) - 2) digits
    opCode = 10 * (head opCodeDigits) + (head $ tail opCodeDigits)
    allModes = padLeft 3 modes
  in (reverse allModes, opCode)

eval :: [Int] -> Int -> Int -> Int
eval memory value 0 = memory !! value
eval memory value 1 = value

execute :: Int -> Int -> ([Int], [Int]) -> ([Int], [Int])
execute input ip (memory, outputs) =
  let
    op = memory !! ip
    (modes, opCode) = parseOp op
  in case opCode of
    99 -> (memory, outputs)
    1  -> let
        a = eval memory (memory !! (ip + 1)) (modes !! 0)
        b = eval memory (memory !! (ip + 2)) (modes !! 1)
        i = memory !! (ip + 3)
      in execute input (ip + 4) $ (setElement i (a + b) memory, outputs)
    2  -> let
        a = eval memory (memory !! (ip + 1)) (modes !! 0)
        b = eval memory (memory !! (ip + 2)) (modes !! 1)
        i = memory !! (ip + 3)
      in execute input (ip + 4) $ (setElement i (a * b) memory, outputs)
    3  -> execute input (ip + 2) $ (setElement (memory !! (ip + 1)) input memory, outputs)
    4  -> execute input (ip + 2) $ (memory, (memory !! (memory !! (ip + 1))) : outputs)
    5 -> let
        b = eval memory (memory !! (ip + 1)) (modes !! 0)
        t = eval memory (memory !! (ip + 2)) (modes !! 1)
        newIp = if b /= 0 then t else (ip + 3)
      in execute input newIp $ (memory, outputs)
    6 -> let
        b = eval memory (memory !! (ip + 1)) (modes !! 0)
        t = eval memory (memory !! (ip + 2)) (modes !! 1)
        newIp = if b == 0 then t else (ip + 3)
      in execute input newIp $ (memory, outputs)
    7 -> let
        a = eval memory (memory !! (ip + 1)) (modes !! 0)
        b = eval memory (memory !! (ip + 2)) (modes !! 1)
        i = memory !! (ip + 3)
        r = if a < b then 1 else 0
      in execute input (ip + 4) $ ((setElement i r memory), outputs)
    8 -> let
        a = eval memory (memory !! (ip + 1)) (modes !! 0)
        b = eval memory (memory !! (ip + 2)) (modes !! 1)
        i = memory !! (ip + 3)
        r = if a == b then 1 else 0
      in execute input (ip + 4) $ ((setElement i r memory), outputs)

main = do
  input <- getContents
  let initialMemory = map read $ splitOn "," input
  print $ "Part 1: " ++ (show $ head $ snd $ execute 1 0 (initialMemory, []))
  print $ "Part 2: " ++ (show $ head $ snd $ execute 5 0 (initialMemory, []))
