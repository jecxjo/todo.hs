module Tasks
  (
    Priority
  , Project
  , Context
  , Date(..)
  , Task(..)
  , onlyPending
  , onlyCompleted
  , filterProjects
  , filterContext
  ) where

import Util (subsetOf)

-- |Priority: (A)
type Priority = Char

-- |Project: +ProjectName
type Project = String

-- |Context: @ContextString
type Context = String

-- |Date: Year, Month, Day
data Date = Date Integer Int Int

-- |Show Date in YYYY-MM-DD format
instance Show Date where
  show (Date year month day) = show year
                               ++ "-" ++ showDoubleDigit month
                               ++ "-" ++ showDoubleDigit day
    where showDoubleDigit num = if num < 10
                                then "0" ++ show num
                                else show num

-- |Check if two Dates are equal
instance Eq Date where
  (Date y1 m1 d1) == (Date y2 m2 d2) = y1 == y2
                                       && m1 == m2
                                       && d1 == d2

-- |Compare (LT,GT,EQ) two Dates
instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2) = if y1 /= y2
                                            then compare y1 y2
                                            else if m1 /= m2
                                                 then compare m1 m2
                                                 else compare d1 d2

-- |Data type to store both incomplete and completed tasks.
data Task = Incomplete (Maybe Priority) (Maybe Date) [Project] [Context] String
          | Completed Date Task

-- |Show Task in format "(A) 2016-07-30 Task to do +Project @Context"
instance Show Task where
  show (Completed date task) = "x " ++ (show date) ++ " " ++ (show task)
  show (Incomplete mPriority mDate _ _ str) = (showPriority mPriority)
                                              ++ (showDate mDate)
                                              ++ str
    where showPriority (Just p) = "(" ++ [p] ++ ") "
          showPriority Nothing = ""
          showDate (Just d) = show d ++ " "
          showDate Nothing = ""

-- |Comparisons are done based on priority
instance Eq Task where
  (Incomplete Nothing _ _ _ _) == (Incomplete Nothing _ _ _ _) = True
  (Incomplete (Just _) _ _ _ _) == (Incomplete Nothing _ _ _ _) = False
  (Incomplete Nothing _ _ _ _) == (Incomplete (Just _) _ _ _ _) = False
  (Incomplete (Just a) _ _ _ _) == (Incomplete (Just b) _ _ _ _) = a == b
  (Incomplete _ _ _ _ _) == (Completed _ _) = False
  (Completed _ _) == (Incomplete _ _ _ _ _) = False
  (Completed _ t1) == (Completed _ t2) = t1 == t2

-- |Comparisons are done based on priority. Since we want A > B, all the
-- comparisons are backwards.
instance Ord Task where
  compare (Incomplete Nothing _ _ _ _) (Incomplete Nothing _ _ _ _) = EQ
  compare (Incomplete (Just _) _ _ _ _) (Incomplete Nothing _ _ _ _) = GT
  compare (Incomplete Nothing _ _ _ _) (Incomplete (Just _) _ _ _ _) = LT
  compare (Incomplete _ _ _ _ _) (Completed _ _) = LT
  compare (Completed _ _) (Incomplete _ _ _ _ _) = GT
  -- The following are backwards on purpose, to get A > B
  compare (Incomplete (Just a) _ _ _ _) (Incomplete (Just b) _ _ _ _) =
    compare b a
  compare (Completed _ t1) (Completed _ t2) = compare t2 t1

-- |Filters out all completed tasks and returns a list of incomplete
onlyPending :: [Task] -> [Task]
onlyPending = filter isPending
  where isPending (Incomplete _ _ _ _ _) = True
        isPending _ = False

-- |Filters out all incomplete tasks and returns a list of completed
onlyCompleted :: [Task] -> [Task]
onlyCompleted = filter isCompleted
  where isCompleted (Completed _ _) = True
        isCompleted _ = False

-- |Filters based on Projects. Strings for project should not contain '+' as
-- defined by todo.txt
filterProjects :: [Project] -> [Task] -> [Task]
filterProjects px = filter projectFilter
  where projectFilter (Incomplete _ _ projs _ _) = px `subsetOf` projs
        projectFilter (Completed _ t) = projectFilter t

-- |Filters based on Context. Strings for context should not contain '@' as
-- defined by todo.txt
filterContext :: [Context] -> [Task] -> [Task]
filterContext cx = filter contextFilter
  where contextFilter (Incomplete _ _ _ ctx _) = cx `subsetOf` ctx
        contextFilter (Completed _ t) = contextFilter t
