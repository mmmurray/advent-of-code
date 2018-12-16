import qualified Data.List.Split as Split
import Text.Regex.PCRE
import Data.Bits

type Registers = [Int]
type Instruction = [Int]
type Operation = Registers -> Instruction -> Registers

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

behavesLikeOperation :: (Registers, Instruction, Registers) -> Operation -> Bool
behavesLikeOperation (before, instruction, after) operation = operation before instruction == after

behavesLikeThreeOrMoreOperations :: [Operation] -> (Registers, Instruction, Registers) -> Bool
behavesLikeThreeOrMoreOperations operations test = length (filter (behavesLikeOperation test) operations) >= 3

parseInput :: String -> [(Registers, Instruction, Registers)]
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

main = do
  contents <- getContents
  let
    testInputs = parseInput contents
    part1 = length (filter (behavesLikeThreeOrMoreOperations operations) testInputs)
  print $ part1
