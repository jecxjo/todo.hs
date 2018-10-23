{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo.Tasks
  (
    Priority
  , Project
  , Context
  , KeyValue(..)
  , StringTypes(..)
  , Task(..)
  , isIncomplete
  , isCompleted
  , containsText
  , convertToDate
  , convertStringTypes
  , getProjects
  , getContexts
  , getKeyValues
  , extractDueDate
  , extractThreshold
  , extractAt
  ) where

import           Control.Monad (liftM)
import           Data.Bool (bool)
import           Data.Char (toLower)
import           Data.Time (toGregorian)
import           Data.Time.Calendar (addDays, Day(..))
import           Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import           Data.Time.LocalTime (TimeOfDay(..), makeTimeOfDayValid)
import           Data.Traversable (forM)
import qualified Data.Text as T
import           Data.Text (Text)
import           Todo.Util (subsetOf, showPaddedNumber)
import           Todo.App (MonadDate, AppError, getDay, throwError, ErrorType(..))

-- |Priority: (A)
type Priority = Char

-- |Project: +ProjectName
type Project = String

-- |Context: @ContextString
type Context = String

-- |Key Value pairs: due:2017-01-02
data KeyValue = KVDueDate Day
              | KVThreshold Day
              | KVAt TimeOfDay
              | KVString String String
              deriving (Eq)

instance Show KeyValue where
  show (KVDueDate date) = "due:" ++ (show date)
  show (KVThreshold date) = "t:" ++ (show date)
  show (KVAt (TimeOfDay hr min _)) = "at:" ++ (showPaddedNumber '0' 2 hr) ++ (showPaddedNumber '0' 2 min)
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

extractDueDate :: [StringTypes] -> Maybe Day
extractDueDate [] = Nothing
extractDueDate ((SKeyValue (KVDueDate d)):_) = Just d
extractDueDate (_:xs) = extractDueDate xs

extractThreshold :: [StringTypes] -> Maybe Day
extractThreshold [] = Nothing
extractThreshold ((SKeyValue (KVThreshold d)):_) = Just d
extractThreshold (_:xs) = extractThreshold xs

extractAt :: [StringTypes] -> Maybe TimeOfDay
extractAt [] = Nothing
extractAt ((SKeyValue (KVAt tod)):_) = Just tod
extractAt (_:xs) = extractAt xs

-- |Data type to store both incomplete and completed tasks.
data Task = Incomplete (Maybe Priority) (Maybe Day) [StringTypes]
          | Completed Day Task

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
  compare (Incomplete _ _ _) (Completed _ _) = LT
  compare (Completed _ _) (Incomplete _ _ _) = GT
  compare (Incomplete (Just a) _ _) (Incomplete (Just b) _ _) =
    compare a b
  compare (Completed _ t1) (Completed _ t2) = compare t1 t2

-- |Filters out all completed tasks and returns a list of incomplete
isIncomplete :: Task -> Bool
isIncomplete (Incomplete _ _ _) = True
isIncomplete _ = False

-- |Filters out all incomplete tasks and returns a list of completed
isCompleted :: Task -> Bool
isCompleted (Completed _ _) = True
isCompleted _ = False

-- | Returns True if task contains the list of text
containsText :: [Text] -> Task -> Bool
containsText terms task = foldl (\res term -> res && (T.toUpper term) `T.isInfixOf` (T.toUpper . T.pack $ show task)) True terms

-- |Convert String to Date
convertToDate :: (AppError m, MonadDate m) => String -> m Day
convertToDate str
  | dayString == "today" = getDay
  | dayString == "tomorrow" = getDay >>= (return . addDays 1)
  | dayString == "yesterday" = getDay >>= (return . addDays (-1))
  | dayString == "monday" = getDay >>= dayOfWeek 1
  | dayString == "mon" = getDay >>= dayOfWeek 1
  | dayString == "tuesday" = getDay >>= dayOfWeek 2
  | dayString == "tue" = getDay >>= dayOfWeek 2
  | dayString == "wednesday" = getDay >>= dayOfWeek 3
  | dayString == "wed" = getDay >>= dayOfWeek 3
  | dayString == "thursday" = getDay >>= dayOfWeek 4
  | dayString == "thu" = getDay >>= dayOfWeek 4
  | dayString == "friday" = getDay >>= dayOfWeek 5
  | dayString == "fri" = getDay >>= dayOfWeek 5
  | dayString == "saturday" = getDay >>= dayOfWeek 6
  | dayString == "sat" = getDay >>= dayOfWeek 6
  | dayString == "sunday" = getDay >>= dayOfWeek 7
  | dayString == "sun" = getDay >>= dayOfWeek 7
  | otherwise = throwError . EMiscError . T.pack $ "invalid relative date '" ++ str ++ "'"
  where
    dayString = map toLower str
    dayOfWeek off day = do
      let (yr,wk,wday) = toWeekDate day
      if (off <= wday)
      then return . addDays 7 $ fromWeekDate yr wk off
      else return $ fromWeekDate yr wk off

-- |Convert all KV Due Dates that are strings into actual dates
convertStringType :: (AppError m, MonadDate m) => StringTypes -> m StringTypes
convertStringType (SKeyValue kv) = do
    case kv of
      KVString "due" val -> do
        converted <- convertToDate val
        return . SKeyValue $ KVDueDate converted
      KVString "t" val -> do
        converted <- convertToDate val
        return . SKeyValue $ KVThreshold converted
      rest ->  do
        return $ SKeyValue rest
convertStringType rest = do
  return rest

convertStringTypes :: (AppError m, MonadDate m) => [StringTypes] -> m [StringTypes]
convertStringTypes alst = forM alst convertStringType
