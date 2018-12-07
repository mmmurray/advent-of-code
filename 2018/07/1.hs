import Text.Regex.PCRE
import qualified Data.Set as Set
import qualified Data.List as List

parseLine :: String -> (Char, Char)
parseLine line =
  let
    matches = line =~ "Step (.) must be finished before step (.) can begin" :: [[String]]
    [dep, step] = tail . head $ matches
  in
    (head step, head dep)

sortDifference :: String -> String -> String
sortDifference a b = List.sort $ Set.toList $ Set.difference (Set.fromList a) (Set.fromList b)

completableStep :: [(Char, Char)] -> Char
completableStep steps =
  let
    (s, d) = unzip steps
  in
    head $ sortDifference d s

stepsWithNoRequirements :: [(Char, Char)] -> String
stepsWithNoRequirements steps =
  let
    (s, d) = unzip steps
  in
    sortDifference s d

getStepSequence :: [(Char, Char)] -> String
getStepSequence steps = f steps ""
  where
    f remainingSteps done =
      let
        completable = completableStep remainingSteps
        newRemainingSteps = filter (\(step, dep) -> dep /= completable) remainingSteps
        newDone = done ++ [completable]
      in
        if length newRemainingSteps == 0 then newDone else f newRemainingSteps newDone

main = do
  contents <- getContents
  let
    allLines = lines contents
    steps = map parseLine allLines
    remainder = stepsWithNoRequirements steps
    sequence = (getStepSequence steps) ++ remainder
  print $ sequence
