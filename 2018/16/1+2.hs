import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Regex.PCRE
import Data.Bits

type Registers = [Int]
type Instruction = [Int]
type Operation = Registers -> Instruction -> Registers
type Sample = (Registers, Instruction, Registers)

parseInput :: String -> [Sample]
parseInput input = map (f . (take 3)) (Split.chunksOf 4 (lines input))
  where
    f l =
      let
        [before, instruction, after] = l :: [String]
        matchesBefore = before =~ "^Before: \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]$" :: [[String]]
        matchesInstruction = instruction =~ "^(\\d+) (\\d+) (\\d+) (\\d+)$" :: [[String]]
        matchesAfter = after =~ "^After:  \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]$" :: [[String]]
        readDigits matches = map read (tail . head $ matches)
      in
        (readDigits matchesBefore, readDigits matchesInstruction, readDigits matchesAfter)

parseProgram :: String -> [Instruction]
parseProgram input = map f (lines input)
  where
    f :: String -> Instruction
    f l =
      let
        matchesInstruction = l =~ "^(\\d+) (\\d+) (\\d+) (\\d+)$" :: [[String]]
      in
        map read (tail . head $ matchesInstruction)

set :: Registers -> Int -> Int -> Registers
set [r0, r1, r2, r3] register value
  | register == 0 = [value, r1, r2, r3]
  | register == 1 = [r0, value, r2, r3]
  | register == 2 = [r0, r1, value, r3]
  | register == 3 = [r0, r1, r2, value]

get :: Registers -> Int -> Int
get [r0, r1, r2, r3] register
  | register == 0 = r0
  | register == 1 = r1
  | register == 2 = r2
  | register == 3 = r3

addr :: Operation
addr registers [operation, a, b, c] = set registers c ((get registers a) + (get registers b))

addi :: Operation
addi registers [operation, a, b, c] = set registers c ((get registers a) + b)

mulr :: Operation
mulr registers [operation, a, b, c] = set registers c ((get registers a) * (get registers b))

muli :: Operation
muli registers [operation, a, b, c] = set registers c ((get registers a) * b)

banr :: Operation
banr registers [operation, a, b, c] = set registers c ((.&.) (get registers a) (get registers b))

bani :: Operation
bani registers [operation, a, b, c] = set registers c ((.&.) (get registers a) b)

borr :: Operation
borr registers [operation, a, b, c] = set registers c ((.|.) (get registers a) (get registers b))

bori :: Operation
bori registers [operation, a, b, c] = set registers c ((.|.) (get registers a) b)

setr :: Operation
setr registers [operation, a, b, c] = set registers c (get registers a)

seti :: Operation
seti registers [operation, a, b, c] = set registers c a

gtir :: Operation
gtir registers [operation, a, b, c] = set registers c (if a > (get registers b) then 1 else 0)

gtri :: Operation
gtri registers [operation, a, b, c] = set registers c (if (get registers a) > b then 1 else 0)

gtrr :: Operation
gtrr registers [operation, a, b, c] = set registers c (if (get registers a) > (get registers b) then 1 else 0)

eqir :: Operation
eqir registers [operation, a, b, c] = set registers c (if a == (get registers b) then 1 else 0)

eqri :: Operation
eqri registers [operation, a, b, c] = set registers c (if (get registers a) == b then 1 else 0)

eqrr :: Operation
eqrr registers [operation, a, b, c] = set registers c (if (get registers a) == (get registers b) then 1 else 0)

operations :: [(Operation)]
operations = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

behavesLikeOperation :: Sample -> Operation -> Bool
behavesLikeOperation (before, instruction, after) operation = operation before instruction == after

behavesLikeThreeOrMoreOperations :: [Operation] -> Sample -> Bool
behavesLikeThreeOrMoreOperations operations sample = length (filter (behavesLikeOperation sample) operations) >= 3

behavesLikeOneOperation :: [Operation] -> Sample -> Bool
behavesLikeOneOperation operations sample = length (filter (behavesLikeOperation sample) operations) == 1

behavesLikeOperations :: [Operation] -> Sample -> [Operation]
behavesLikeOperations operations sample = filter (behavesLikeOperation sample) operations

getOpCode :: Sample -> Int
getOpCode (_, instruction, _) =
  let
    [opCode, _, _, _] = instruction
  in
    opCode

possibleOperations :: [Operation] -> Sample -> Set.Set Int
possibleOperations operations sample =
  let
    indexedOperations = zip [0..] operations
    possible = filter (\(i, operation) -> behavesLikeOperation sample operation) indexedOperations
  in Set.fromList (map fst possible)

translateOpCode :: [Operation] -> [Sample] -> Int -> [Int]
translateOpCode operations samples opCode =
  let
    samplesForOperation = filter (\sample -> getOpCode sample == opCode) samples
    operationSets = map (possibleOperations operations) samplesForOperation
    intersection = foldl (\acc s -> Set.intersection acc s) (Set.fromList [0..(length operations - 1)]) operationSets
  in Set.toList intersection

-- Generated manually by analysing Mappings output using Sudoku style strategy
operationMap :: Map.Map Int Operation
operationMap = Map.fromList [
    (0, operations !! 5),
    (1, operations !! 11),
    (2, operations !! 9),
    (3, operations !! 13),
    (4, operations !! 15),
    (5, operations !! 6),
    (6, operations !! 7),
    (7, operations !! 4),
    (8, operations !! 3),
    (9, operations !! 14),
    (10, operations !! 2),
    (11, operations !! 12),
    (12, operations !! 8),
    (13, operations !! 0),
    (14, operations !! 10),
    (15, operations !! 1)
  ]

runProgram :: Map.Map Int Operation -> [Instruction] -> Registers
runProgram operationMap instructions =
  foldl (\acc instruction -> (operationMap Map.! (head instruction)) acc instruction) [0,0,0,0] instructions

main = do
  contents <- getContents
  program <- readFile "program.txt"
  let
    programInstructions = parseProgram program
    samples = parseInput contents
    part1 = length (filter (behavesLikeThreeOrMoreOperations operations) samples)
    part2 = head $ runProgram operationMap programInstructions
  putStr $ "Mappings: " ++ (unlines $ map (\i -> (show i) ++ ": " ++ (show $ translateOpCode operations samples i)) [0..15])
  print ("Part 1: " ++ (show part1))
  print ("Part 2: " ++ (show part2))
