fuelRequired :: Int -> Int
fuelRequired mass = max 0 (mass `div` 3 - 2)

fuelRequiredRec :: Int -> Int
fuelRequiredRec 0 = 0
fuelRequiredRec m = fuelRequired m + fuelRequiredRec (fuelRequired m)

main = do
  contents <- getContents
  let
    masses = map read (lines contents)
  print $ "Part 1: " ++ (show $ sum . map fuelRequired $ masses)
  print $ "Part 2: " ++ (show $ sum . map fuelRequiredRec $ masses)
