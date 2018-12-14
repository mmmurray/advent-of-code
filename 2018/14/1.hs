type Elf = (Int, Int)


parseDigit :: Char -> Int
parseDigit c = read [c]


newCurrent :: [Int] -> Int -> Elf -> Elf
newCurrent scoreboard offset (index, value) =
  let
    newIndex = ((index + offset) - value - 1) `mod` (length scoreboard)
    newValue = (!!) scoreboard newIndex
  in
    (newIndex, newValue)


createRecipes :: ([Int], [Elf]) -> ([Int], [Elf])
createRecipes (scoreboard, elves) =
  let
    current = map snd elves
    result = sum current

    (newScoreboard, offset) = if result >= 10
      then ((result - 10) : 1 : scoreboard, 2)
      else (result : scoreboard, 1)

    newElves = map (newCurrent newScoreboard offset) elves
  in
    (newScoreboard, newElves)


recipes :: ([Int], [Elf]) -> Int -> [Int]
recipes (scoreboard, elves) n
  | length scoreboard < n = recipes (createRecipes (scoreboard, elves)) n
  | otherwise = scoreboard


main = do
  contents <- getContents
  let
    count = 47801
    scoreboard = recipes ([7, 3], [(1, 3), (0, 7)]) (count + 10)
    result = take 10 $ drop count (reverse scoreboard)
  print $ "Part 1: " ++ map (head . show) result
