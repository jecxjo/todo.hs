module Tasks
  (
    Priority
  , Project
  , Context
  , Date(..)
  , KeyValue(..)
  , StringTypes(..)
  , Task(..)
  , onlyPending
  , onlyCompleted
  , filterProjects
  , filterContext
  , convertToDate
  , convertStringTypes
  , getProjects
  , getContexts
  , getKeyValues
  , extractDueDate
  ) where

import Util (subsetOf, MonadDate(..), getToday)

import Control.Monad.Trans.Class (lift)
import Data.Char (toLower)
import Data.List (find)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Data.Time.Calendar (addDays)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Traversable (forM)

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

-- |Key Value pairs: due:2017-01-02
data KeyValue = KVDueDate Date
              | KVString String String
              deriving (Eq)

instance Show KeyValue where
  show (KVDueDate date) = "due:" ++ (show date)
  show (KVString key value) = key ++ ":" ++ value


-- |String Types
data StringTypes = SProject Project
                 | SContext Context
                 | SKeyValue KeyValue
                 | SOther String
                 deriving Eq

instance Show StringTypes where
  show (SProject p) = "+" ++ p
  show (SContext c) = "@" ++ c
  show (SKeyValue kv) = show kv
  show (SOther s) = s

unrollStringTypes :: [StringTypes] -> String
unrollStringTypes = unwords . map show

getProjects :: [StringTypes] -> [Project]
getProjects = map (\(SProject p) -> p) . filter fn
  where fn (SProject _) = True
        fn _ = False

getContexts :: [StringTypes] -> [Context]
getContexts = map (\(SContext c) -> c) . filter fn
  where fn (SContext _) = True
        fn _ = False

getKeyValues :: [StringTypes] -> [KeyValue]
getKeyValues = map (\(SKeyValue kv) -> kv) . filter fn
  where fn (SKeyValue _) = True
        fn _ = False

extractDueDate :: [StringTypes] -> Maybe Date
extractDueDate [] = Nothing
extractDueDate ((SKeyValue (KVDueDate d)):_) = Just d
extractDueDate (_:xs) = extractDueDate xs

-- |Data type to store both incomplete and completed tasks.
data Task = Incomplete (Maybe Priority) (Maybe Date) [StringTypes]
          | Completed Date Task

-- |Show Task in format "(A) 2016-07-30 Task to do +Project @Context"
instance Show Task where
  show (Completed date task) = "x " ++ (show date) ++ " " ++ (show task)
  show (Incomplete mPriority mDate sx) = (showPriority mPriority)
                                                ++ (showDate mDate)
                                                ++ (unrollStringTypes sx)
    where showPriority (Just p) = "(" ++ [p] ++ ") "
          showPriority Nothing = ""
          showDate (Just d) = show d ++ " "
          showDate Nothing = ""

-- |Comparisons are done based on priority
instance Eq Task where
  (Incomplete Nothing _ _) == (Incomplete Nothing _ _) = True
  (Incomplete (Just _) _ _) == (Incomplete Nothing _ _) = False
  (Incomplete Nothing _ _) == (Incomplete (Just _) _ _) = False
  (Incomplete (Just a) _ _) == (Incomplete (Just b) _ _) = a == b
  (Incomplete _ _ _) == (Completed _ _) = False
  (Completed _ _) == (Incomplete _ _ _) = False
  (Completed _ t1) == (Completed _ t2) = t1 == t2

-- |Comparisons are done based on priority.
instance Ord Task where
  compare (Incomplete Nothing _ _) (Incomplete Nothing _ _) = EQ
  compare (Incomplete (Just _) _ _) (Incomplete Nothing _ _) = LT
  compare (Incomplete Nothing _ _) (Incomplete (Just _) _ _) = GT
  compare (Incomplete _ _ _) (Completed _ _) = GT
  compare (Completed _ _) (Incomplete _ _ _) = LT
  compare (Incomplete (Just a) _ _) (Incomplete (Just b) _ _) =
    compare a b
  compare (Completed _ t1) (Completed _ t2) = compare t1 t2

-- |Filters out all completed tasks and returns a list of incomplete
onlyPending :: [Task] -> [Task]
onlyPending = filter isPending
  where isPending (Incomplete _ _ _) = True
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
  where projectFilter (Incomplete _ _ sx) = px `subsetOf` (getProjects sx)
        projectFilter (Completed _ t) = projectFilter t

-- |Filters based on Context. Strings for context should not contain '@' as
-- defined by todo.txt
filterContext :: [Context] -> [Task] -> [Task]
filterContext cx = filter contextFilter
  where contextFilter (Incomplete _ _ sx) = cx `subsetOf` (getContexts sx)
        contextFilter (Completed _ t) = contextFilter t

-- |Convert String to Date
convertToDate :: MonadDate m => String -> m Date
convertToDate str = do
    let dateStr = map toLower str
    let dateNum = toNum dateStr
    today <- getToday
    let (yr,wk,wday) = toWeekDate $ today
    case dateStr of
      "today" -> return $ genDate $ toGregorian $ today
      "tomorrow" -> return $ genDate $ toGregorian $ addDays 1 $ today
      "yesterday" -> return $ genDate $ toGregorian $ addDays (-1) $ today
      _       -> if dateNum <= wday
                 then do
                    return $ genDate $ toGregorian $ addDays 7 $ fromWeekDate yr wk dateNum
                 else do
                    return $ genDate $ toGregorian $ fromWeekDate yr wk dateNum
  where
    toNum "monday" = 1
    toNum "tuesday" = 2
    toNum "wednesday" = 3
    toNum "thursday" = 4
    toNum "friday" = 5
    toNum "saturday" = 6
    toNum "sunday" = 7
    toNum _ = error "Bad Due Date"
    genDate (y,m,d) = Date y m d

-- |Convert all KV Due Dates that are strings into actual dates
convertStringType :: MonadDate m => StringTypes -> m StringTypes
convertStringType (SKeyValue kv) = do
    case kv of
      KVString "due" val -> do
        converted <- convertToDate val
        return . SKeyValue $ KVDueDate converted
      rest ->  do
        return $ SKeyValue rest
convertStringType rest = do
  return rest

convertStringTypes :: MonadDate m => [StringTypes] -> m [StringTypes]
convertStringTypes alst = forM alst convertStringType
