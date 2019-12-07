import Data.Char (digitToInt)
import Data.List (permutations)
import Data.List.Split (splitOn)

type State = (Int, [Int], [Int], Int, Bool)

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs) ++ x : (drop (i + 1) xs)

padLeft :: Int -> [Int] -> [Int]
padLeft l d = (replicate (l - length d) 0) ++ d

parseOp :: Int -> ([Int], Int)
parseOp op =
  let digits       = map digitToInt $ show op
      modes        = take ((length digits) - 2) digits
      opCodeDigits = padLeft 2 $ drop ((length digits) - 2) digits
      opCode       = 10 * (head opCodeDigits) + (head $ tail opCodeDigits)
      allModes     = padLeft 3 modes
  in  (reverse allModes, opCode)

eval :: [Int] -> Int -> Int -> Int
eval memory value 0 = memory !! value
eval memory value 1 = value

execute :: Int -> [Int] -> Int -> ([Int], [Int]) -> State
execute id inputs ip (memory, outputs) =
  let
    op              = memory !! ip
    (modes, opCode) = parseOp op
  in
    case opCode of
      99 -> (id, memory, outputs, ip, True)
      1 ->
        let a = eval memory (memory !! (ip + 1)) (modes !! 0)
            b = eval memory (memory !! (ip + 2)) (modes !! 1)
            i = memory !! (ip + 3)
        in  execute id inputs (ip + 4) $ (setElement i (a + b) memory, outputs)
      2 ->
        let a = eval memory (memory !! (ip + 1)) (modes !! 0)
            b = eval memory (memory !! (ip + 2)) (modes !! 1)
            i = memory !! (ip + 3)
        in  execute id inputs (ip + 4) $ (setElement i (a * b) memory, outputs)
      3 -> if length inputs > 0 then
        (execute id (tail inputs) (ip + 2)
          $ (setElement (memory !! (ip + 1)) (head inputs) memory, outputs)) else (id, memory, outputs, ip, False)
      4 ->
        execute id inputs (ip + 2)
          $ (memory, (memory !! (memory !! (ip + 1))) : outputs)
      5 ->
        let b     = eval memory (memory !! (ip + 1)) (modes !! 0)
            t     = eval memory (memory !! (ip + 2)) (modes !! 1)
            newIp = if b /= 0 then t else (ip + 3)
        in  execute id inputs newIp $ (memory, outputs)
      6 ->
        let b     = eval memory (memory !! (ip + 1)) (modes !! 0)
            t     = eval memory (memory !! (ip + 2)) (modes !! 1)
            newIp = if b == 0 then t else (ip + 3)
        in  execute id inputs newIp $ (memory, outputs)
      7 ->
        let a = eval memory (memory !! (ip + 1)) (modes !! 0)
            b = eval memory (memory !! (ip + 2)) (modes !! 1)
            i = memory !! (ip + 3)
            r = if a < b then 1 else 0
        in  execute id inputs (ip + 4) $ ((setElement i r memory), outputs)
      8 ->
        let a = eval memory (memory !! (ip + 1)) (modes !! 0)
            b = eval memory (memory !! (ip + 2)) (modes !! 1)
            i = memory !! (ip + 3)
            r = if a == b then 1 else 0
        in  execute id inputs (ip + 4) $ ((setElement i r memory), outputs)

tick :: [State] -> Int
tick (a:b:states) = let
    (_, _, inputs, _, _) = a
    (id, memory, outputs, ip, halted) = b
    newB = if halted then b else execute id inputs ip (memory, [])
  in if id == 4 && halted then (head outputs) else tick (newB:states ++ [a])

amplify :: [Int] -> [Int] -> Int
amplify initialMemory phaseSettings = tick initialStates
  where
    initialInputs 0 = [(phaseSettings !! 0), 0]
    initialInputs id = [(phaseSettings !! id)]
    initialState id = execute id (initialInputs id) 0 (initialMemory, [])
    initialStates = map initialState [0..4]

maximizeSignal :: [Int] -> [Int] -> Int
maximizeSignal initialMemory settings =
  maximum $ map (amplify initialMemory) $ permutations settings

main = do
  input <- getContents
  let initialMemory = map read $ splitOn "," input
  print $ "Part 1: " ++ (show $ maximizeSignal initialMemory [0..4])
  print $ "Part 2: " ++ (show $ maximizeSignal initialMemory [5..9])
