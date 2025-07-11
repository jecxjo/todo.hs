module Todo.Commands.Burndown
  ( calculateActiveTasks
  , calculateActiveTasksNormalized
  ) where

import Todo.Tasks (Task(..))
import Data.Time.Calendar (Day)

-- | Calculate the number of active tasks for each day within a given range
calculateActiveTasks :: [Task] -> Day -> Day -> [(Day, Int)]
calculateActiveTasks tasks start end =
    let days = [start .. end]
    in map (\day -> (day, countActiveTasks day tasks)) days

-- | Count the number of active tasks for a specific day
countActiveTasks :: Day -> [Task] -> Int
countActiveTasks day tasks =
    length $ filter (\task -> isActiveOn day task) tasks

-- | Check if a task is active on a specific day
isActiveOn :: Day -> Task -> Bool
isActiveOn day (Incomplete _ (Just start) _) = start <= day 
isActiveOn day (Completed _ end start _) = start <= (Just day) && end >= (Just day)
isActiveOn _ _ = False


-- | Calculate the number of actives tasks for each day and normalize the values
calculateActiveTasksNormalized :: Int -> [Task] -> Day -> Day -> [(Day, Int)]
calculateActiveTasksNormalized norm tasks start end =
    let activeTasks = calculateActiveTasks tasks start end
        maxActive = maximum $ map snd activeTasks
        ratio = if maxActive <= norm then (\x -> x) else (\x -> (norm * x) `div` maxActive)
        in map (\(day, count) -> (day, ratio count)) activeTasks
