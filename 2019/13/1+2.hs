import Data.Char (digitToInt)
import Data.List (intercalate, permutations)
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShow, traceShowId)

type Screen = Map Point Value

type Value = Integer
type Point = (Value, Value)
type Grid = Map Point Value
type Memory = Map Value Value
type Inputs = [Value]
type Outputs = [Value]
-- memory, inputs, outputs, ip, relativeBase, halted
type State = (Memory, Inputs, Outputs, Value, Value, Bool)

-- |
-- >>> parseMemory "-7,8,9"
-- fromList [(0,-7),(1,8),(2,9)]
parseMemory :: String -> Memory
parseMemory input = Map.fromList $ zip [0..] (map read $ splitOn "," input)

-- |
-- >>> memRead mempty 9
-- 0
-- >>> memRead (Map.fromList [(0,7),(1,8),(2,9)]) 1
-- 8
memRead :: Memory -> Value -> Value
memRead memory address = case Map.lookup address memory of
  Just v -> v
  Nothing -> 0

-- |
-- >>> memWrite 3 2 mempty
-- fromList [(3,2)]
-- >>> memWrite 1 5 (Map.fromList [(0,7),(1,8),(2,9)])
-- fromList [(0,7),(1,5),(2,9)]
-- >>> memWrite (-1) 5 (Map.fromList [(0,7),(1,8),(2,9)])
-- fromList [(-1,5),(0,7),(1,8),(2,9)]
memWrite :: Value -> Value -> Memory -> Memory
memWrite address value memory = Map.insert address value memory

-- |
-- >>> padLeft 3 [1]
-- [0,0,1]
-- >>> padLeft 3 [1,2,3]
-- [1,2,3]
-- >>> padLeft 0 []
-- []
padLeft :: Int -> [Int] -> [Int]
padLeft l d = (replicate (l - length d) 0) ++ d

-- |
-- >>> parseOp 101
-- ([1,0,0],1)
-- >>> parseOp 5
-- ([0,0,0],5)
-- >>> parseOp 11104
-- ([1,1,1],4)
parseOp :: Value -> ([Int], Int)
parseOp op =
  let digits       = map digitToInt $ show op
      modes        = take ((length digits) - 2) digits
      opCodeDigits = padLeft 2 $ drop ((length digits) - 2) digits
      opCode       = 10 * (head opCodeDigits) + (head $ tail opCodeDigits)
      allModes     = padLeft 3 modes
  in  (reverse allModes, opCode)

-- |
-- >>> eval (Map.fromList [(0,7),(1,8),(2,9)]) 1 0 1
-- 8
-- >>> eval (Map.fromList [(0,7),(1,8),(2,9)]) 1 1 1
-- 1
-- >>> eval (Map.fromList [(0,7),(1,8),(2,9)]) 1 2 1
-- 9
eval :: Memory -> Value -> Int -> Value -> Value
eval memory value 0 relativeBase = memRead memory value
eval memory value 1 relativeBase = value
eval memory value 2 relativeBase = memRead memory $ value + relativeBase

execute :: State -> State
execute (memory, inputs, outputs, ip, relativeBase, halted) =
  let
    op              = memRead memory ip
    (modes, opCode) = parseOp op
    param :: Int -> Integer
    param i = eval memory (memRead memory (ip + (toInteger i) + 1)) (modes !! i) relativeBase
    addr i = let
        r = memRead memory (ip + (toInteger i) + 1)
        m = (modes !! i)
      in
        if m == 2 then relativeBase + r else r
  in
    case (opCode) of
      99 -> (memory, inputs, outputs, ip, relativeBase, True)
      1 ->
        let a = param 0
            b = param 1
            i = addr 2
        in  execute (memWrite i (a + b) memory, inputs, outputs, ip + 4, relativeBase, halted)
      2 ->
        let a = param 0
            b = param 1
            i = addr 2
        in  execute (memWrite i (a * b) memory, inputs, outputs, ip + 4, relativeBase, halted)
      3 -> if length inputs > 0
           then
              let i = addr 0
              in  execute (memWrite i (head inputs) memory, tail inputs, outputs, ip + 2, relativeBase, halted)
          else (memory, inputs, outputs, ip, relativeBase, False)
      4 ->
        let output = param 0
        in  execute (memory, inputs, output : outputs, ip + 2, relativeBase, halted)
      5 ->
        let b     = param 0
            t     = param 1
            newIp = if b /= 0 then t else (ip + 3)
        in  execute (memory, inputs, outputs, newIp, relativeBase, halted)
      6 ->
        let b     = param 0
            t     = param 1
            newIp = if b == 0 then t else (ip + 3)
        in  execute (memory, inputs, outputs, newIp, relativeBase, halted)
      7 ->
        let a = param 0
            b = param 1
            i = addr 2
            r = if a < b then 1 else 0
        in  execute (memWrite i r memory, inputs, outputs, ip + 4, relativeBase, halted)
      8 ->
        let a = param 0
            b = param 1
            i = addr 2
            r = if a == b then 1 else 0
        in  execute (memWrite i r memory, inputs, outputs, ip + 4, relativeBase, halted)
      9 ->
        let offset = param 0
        in  execute (memory, inputs, outputs, ip + 2, relativeBase + offset, halted)

-- |
-- >>> run [] "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
-- [99,0,101,1006,101,16,100,1008,100,1,100,1001,-1,204,1,109]
-- >>> run [] "1102,34915192,34915192,7,4,7,99,0"
-- [1219070632396864]
-- >>> run [] "104,1125899906842624,99"
-- [1125899906842624]
-- >>> run [7] "3,9,8,9,10,9,4,9,99,-1,8"
-- [0]
-- >>> run [8] "3,9,8,9,10,9,4,9,99,-1,8"
-- [1]
-- >>> run [9] "3,9,8,9,10,9,4,9,99,-1,8"
-- [0]
-- >>> run [7] "3,9,7,9,10,9,4,9,99,-1,8"
-- [1]
-- >>> run [8] "3,9,7,9,10,9,4,9,99,-1,8"
-- [0]
-- >>> run [9] "3,9,7,9,10,9,4,9,99,-1,8"
-- [0]
-- >>> run [7] "3,3,1108,-1,8,3,4,3,99"
-- [0]
-- >>> run [8] "3,3,1108,-1,8,3,4,3,99"
-- [1]
-- >>> run [9] "3,3,1108,-1,8,3,4,3,99"
-- [0]
-- >>> run [7] "3,3,1107,-1,8,3,4,3,99"
-- [1]
-- >>> run [8] "3,3,1107,-1,8,3,4,3,99"
-- [0]
-- >>> run [9] "3,3,1107,-1,8,3,4,3,99"
-- [0]
-- >>> run [0] "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
-- [0]
-- >>> run [1] "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
-- [1]
-- >>> run [0] "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
-- [0]
-- >>> run [1] "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
-- [1]
-- >>> run [7] "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
-- [999]
-- >>> run [8] "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
-- [1000]
-- >>> run [9] "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
-- [1001]
-- >>> run [1] "3,225,1,225,6,6,1100,1,238,225,104,0,1101,37,34,224,101,-71,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1002,113,50,224,1001,224,-2550,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,13,50,225,102,7,187,224,1001,224,-224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,79,72,225,1101,42,42,225,1102,46,76,224,101,-3496,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1102,51,90,225,1101,11,91,225,1001,118,49,224,1001,224,-140,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,191,87,224,1001,224,-1218,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1,217,83,224,1001,224,-124,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,32,77,225,1101,29,80,225,101,93,58,224,1001,224,-143,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1101,45,69,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,374,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,419,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,434,1001,223,1,223,107,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,1108,677,677,224,1002,223,2,223,1006,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,509,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1007,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,569,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,614,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,1008,226,677,224,102,2,223,223,1005,224,644,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,659,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226"
-- [13294380,0,0,0,0,0,0,0,0,0]
-- >>> run [5] "3,225,1,225,6,6,1100,1,238,225,104,0,1101,37,34,224,101,-71,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1002,113,50,224,1001,224,-2550,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,13,50,225,102,7,187,224,1001,224,-224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,79,72,225,1101,42,42,225,1102,46,76,224,101,-3496,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1102,51,90,225,1101,11,91,225,1001,118,49,224,1001,224,-140,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,191,87,224,1001,224,-1218,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1,217,83,224,1001,224,-124,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,32,77,225,1101,29,80,225,101,93,58,224,1001,224,-143,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1101,45,69,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,374,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,419,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,434,1001,223,1,223,107,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,1108,677,677,224,1002,223,2,223,1006,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,509,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1007,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,569,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,614,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,1008,226,677,224,102,2,223,223,1005,224,644,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,659,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226"
-- [11460760]
run :: [Integer] -> String -> [Value]
run inputs input = let
    initialState = (parseMemory input, inputs, [], 0, 0, False)
    (_, _, outputs, _, _, halted) = execute initialState
  in if halted then outputs else []

getJoystickInput :: Screen -> Value
getJoystickInput screen =
  let
    ballX = getBallX screen
    paddleX = getPaddleX screen
  in if ballX == paddleX then 0 else (if ballX < paddleX then -1 else 1)

runGame :: String -> Screen
runGame input =
  let
    initialMemory = memWrite 0 2 $ parseMemory input
    initialState = (initialMemory, [], [], 0, 0, False)
  in f initialState mempty
    where f (memory, _, _, ip, relativeBase, _) screen =
            let
              inputs = if screen == mempty then [] else [getJoystickInput screen]
              newState = execute (memory, inputs, [], ip, relativeBase, False)
              (_,_,outputs,_,_,halted) = newState
              newScreen = updateScreen screen (reverse outputs)
            in if halted
                 then newScreen
                 else f newState newScreen

countBlocks :: Screen -> Int
countBlocks screen = length $ filter (((==) 2) . snd) $ Map.toList screen

getBallX :: Screen -> Value
getBallX screen = fst $ fst $ head $ filter (((==) 4) . snd) $ Map.toList screen

getPaddleX :: Screen -> Value
getPaddleX screen = fst $ fst $ head $ filter (((==) 3) . snd) $ Map.toList screen

updateScreen :: Screen -> [Value] -> Screen
updateScreen screen d =
  let
    points = map (\[x,y,t] -> ((x,y), t)) $ chunksOf 3 d
  in Map.union (Map.fromList points) screen

main = do
  input <- getContents
  let outputs = run [] input
  let screen = updateScreen mempty $ reverse outputs
  print $ "Part 1: " ++ (show $ countBlocks $ screen)
  print $ "Part 2: " ++ (show $ runGame input Map.! (-1,0))
