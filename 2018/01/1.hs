lineToNumber :: String -> Int
lineToNumber (sign:digits) =
  let magnitude = read digits
  in if sign == '+'
    then magnitude
    else -magnitude

parseFequencies :: String -> [Int]
parseFequencies input =
  map lineToNumber (lines input)

calculateFrequency :: [Int] -> Int
calculateFrequency frequencies = foldl (+) 0 frequencies

run :: String -> Int
run input = calculateFrequency . parseFequencies $ input

main = do
  contents <- getContents
  print . run $ contents
