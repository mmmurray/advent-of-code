parseDigit :: Char -> Int
parseDigit c = read [c]


newCurrent :: [Int] -> Int -> Int
newCurrent scoreboard current =
  let
    value = (!!) scoreboard current
  in
    (current + value + 1) `mod` (length scoreboard)


createRecipes :: ([Int], [Int]) -> ([Int], [Int])
createRecipes (scoreboard, elves) =
  let
    current = map ((!!) scoreboard) elves
    newRecipes = map parseDigit $ show $ sum current
    newScoreboard = scoreboard ++ newRecipes
    newElves = map (newCurrent newScoreboard) elves
  in
    (newScoreboard, newElves)


recipes :: ([Int], [Int]) -> Int -> [Int]
recipes (scoreboard, elves) n
  | length scoreboard < n = recipes (createRecipes (scoreboard, elves)) n
  | otherwise = scoreboard


main = do
  contents <- getContents
  let
    count = 47801
    scoreboard = recipes ([3, 7], [0, 1]) (count + 10)
    result = take 10 $ drop count scoreboard
  print result
