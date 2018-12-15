type Elf = (Int, Int)


parseDigit :: Char -> Int
parseDigit c = read [c]


newElf :: [Int] -> Int -> Elf -> Elf
newElf scoreboard offset (index, value) =
  let
    newIndex = ((index + offset) - value - 1) `mod` (length scoreboard)
    newValue = (!!) scoreboard newIndex
  in
    (newIndex, newValue)


createRecipes :: ([Int], [Elf]) -> ([Int], [Elf])
createRecipes (scoreboard, elves) =
  let
    result = sum $ map snd elves

    (newScoreboard, offset) = if result >= 10
      then ((result - 10) : 1 : scoreboard, 2)
      else (result : scoreboard, 1)

    newElves = map (newElf newScoreboard offset) elves
  in
    (newScoreboard, newElves)


recipes :: ([Int], [Elf]) -> Int -> [Int]
recipes (scoreboard, elves) n
  | length scoreboard < n = recipes (createRecipes (scoreboard, elves)) n
  | otherwise = scoreboard


recipesUntil :: ([Int], [Elf]) -> [Int] -> [Int]
recipesUntil (scoreboard, elves) match
  | (take (length match) scoreboard) /= match = recipesUntil (createRecipes (scoreboard, elves)) match
  | otherwise = scoreboard


main = do
  contents <- getContents
  let
    count = 47801
    initial = ([7, 3], [(1, 3), (0, 7)])
    scoreboard = recipes initial (count + 10)
    result = take 10 $ drop count (reverse scoreboard)
    part1 = map (head . show) result

    match = [0, 4, 7, 8, 0, 1]
    part2 = length (drop (length match) (recipesUntil initial (reverse match)))
  print $ "Part 1: " ++ part1
  print $ "Part 2: " ++ show part2
