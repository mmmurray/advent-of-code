import Text.Regex.PCRE
import Data.List
import Data.List.Split

isShiftStart :: String -> Bool
isShiftStart log = log =~ "Guard #(\\d+) begins shift"

extractId :: String -> Int
extractId input =
  let
    matches = input =~ "Guard #(\\d+) begins shift"
  in
    read . last . head $ matches

extractTime :: String -> (Int, Int, Int)
extractTime input =
  let
    matches = input =~ "\\[\\d{4}-(\\d{2})-(\\d{2}) \\d{2}:(\\d{2})\\]" :: [[String]]
    [month, day, minute] = map read $ tail . head $ matches
  in
    (month, day, minute)

getGuardShifts :: [String] -> [Int]
getGuardShifts = foldr (\log acc -> if isShiftStart log then (extractId log) : acc else acc) []

rangeFromLogs :: [String] -> (Int, Int)
rangeFromLogs [a, b] =
  let
    (month1, day1, minute1) = extractTime a
    (month2, day2, minute2) = extractTime b
  in
    (minute1, minute2)

asleepRanges :: [String] -> [(Int, Int)]
asleepRanges logs =
  let
    rangesLogs = chunksOf 2 logs
    ranges = map rangeFromLogs rangesLogs
  in
    ranges

allMinutesInRanges :: [(Int, Int)] -> [Int]
allMinutesInRanges = concatMap (\(start, end) -> [start..(end - 1)])

guardLogsToLog :: (Int, [(Int, Int)]) -> [(Int, (Int, Int))]
guardLogsToLog (guard, logs) = map (\log -> (guard, log)) logs

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

checksum :: (Int, Int, Int) -> Int
checksum (a,b,c) = a * c

main = do
  contents <- getContents
  let
    logs = sort . lines $ contents
    guardShifts = getGuardShifts logs
    logsByShift = tail $ splitWhen isShiftStart logs
    asleepRangesByShift = map asleepRanges logsByShift
    guardLogs = zip guardShifts asleepRangesByShift
    b = concatMap id (map guardLogsToLog guardLogs)
    c = groupBy (\(guard1, r1) (guard2, r2) -> guard1 == guard2) $ sort b
    d = map (\r -> let (ids, ranges) = unzip r in (head ids, ranges)) c

    e = map (\(g, rs) ->
      let
        allMinutes = allMinutesInRanges rs
        minuteGroups = group $ sort allMinutes
        maxGroup = maximumBy (\g1 g2 -> compare (length g1) (length g2)) minuteGroups
        maxMinute = head maxGroup
      in
        (g, length maxGroup, maxMinute)) d

    f = maximumBy (\(a1, a2, a3) (b1, b2, b3) -> compare a2 b2) e
    g = checksum f

  print g
