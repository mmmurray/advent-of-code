import Text.Regex.PCRE
import qualified Data.Set as Set
import Data.List as List
import Debug.Trace

type Task = Char
type Dependency = (Task, Task)
type Worker = (Task, Int)

noTask = '.'

parseLine :: String -> Dependency
parseLine line =
  let
    matches = line =~ "Step (.) must be finished before step (.) can begin" :: [[String]]
    [dep, step] = tail . head $ matches
  in
    (head step, head dep)


allTasks :: [Dependency] -> [Task]
allTasks depdendencies =
  let
    (a, b) = unzip depdendencies
  in
    List.sort $ List.nub $ a ++ b


canStart :: [Dependency] -> [Task] -> Task -> Bool
canStart depdendencies completed task =
  let
    depdendenciesForTask = filter ((== task) . fst) depdendencies
    d = map snd depdendenciesForTask
  in
    List.all (\dep -> elem dep completed) d


sortWorkers :: [Worker] -> [Worker]
sortWorkers workers = List.sortBy (\(task1, time1) (task2, time2) -> compare time1 time2) workers


doWork :: [Worker] -> ([Task], [Worker])
doWork workers =
  let
    timeAdvanced = map (\(task, time) -> (task, max 0 (time - 1))) workers
    completedWorkers = filter (\(task, time) -> time == 0) timeAdvanced
    completedTasks = filter (/= noTask) $ map (\(task, time) -> task) completedWorkers
    newWorkers = map (\(task, time) -> if time == 0 then (noTask, time) else (task, time)) timeAdvanced
  in
    (completedTasks, newWorkers)


taskTime :: Task -> Int
taskTime task =
  case List.elemIndex task ['A'..'Z'] of
    Just index -> index + 61
    Nothing -> 0


assignWork :: [Dependency] -> [Task] -> [Task] -> [Worker] -> ([Task], [Worker])
assignWork depdendencies tasks completed workers =
  let
    freeWorkers = length $ filter (\(task, time) -> task == noTask) workers
    tasksBeingWorkedOn = map fst workers
    availableTasks = tasks \\ tasksBeingWorkedOn
    startableTasks = filter (canStart depdendencies completed) availableTasks
    completable = take freeWorkers startableTasks
    remainingTasks = filter (\t -> not $ t `elem` completable) tasks
    taskQueue = completable ++ [noTask | x <- [0..]]
    taskAssignments = zip (sortWorkers workers) taskQueue
    newWorkers = map (\((task, time), newTask) -> if newTask == noTask then (task, time) else (newTask, (taskTime newTask))) taskAssignments
  in
    (remainingTasks, newWorkers)


allWorkersFinished :: [Worker] -> Bool
allWorkersFinished workers = List.all (\(task, time) -> task == noTask) workers


performGruellingTasks :: [Dependency] -> [Task] -> [Task] -> [Worker] -> Int -> Int
performGruellingTasks depdendencies completed tasks workers time =
  let
    (newCompleted, workers2) = doWork workers
    allCompleted = completed ++ newCompleted
    (tasks2, workers3) = assignWork depdendencies tasks allCompleted workers2
  in
    if allWorkersFinished workers3
      then time + 1
      else performGruellingTasks depdendencies allCompleted tasks2 workers3 (time + 1)


main = do
  contents <- getContents
  let
    allLines = lines contents
    depdendencies = map parseLine allLines
    tasks = allTasks depdendencies
    workers = take 5 $ repeat (noTask, 0)
    time = performGruellingTasks depdendencies [] tasks workers 0
  print $ time - 1 -- Account for coffee break
