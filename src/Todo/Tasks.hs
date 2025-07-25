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

import           Data.Char (toLower)
import           Data.Time.Calendar (addDays, Day(..))
import           Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import           Data.Time.LocalTime (TimeOfDay(..), midday, midnight)
import           Data.Traversable (forM)
import qualified Data.Text as T
import           Data.Text (Text)
import           System.Console.Pretty (Color (..), bgColor, color)
import           Text.Color (ShowColor(..))
import           Todo.Util (showPaddedNumber)
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
  show (KVDueDate date) = "due:" ++ show date
  show (KVThreshold date) = "t:" ++ show date
  show (KVAt (TimeOfDay hr mn _)) = "at:" ++ showPaddedNumber '0' 2 hr ++ showPaddedNumber '0' 2 mn
  show (KVString key value) = key ++ ":" ++ value

instance ShowColor KeyValue where
  showColor (KVDueDate date) = bgColor Red $ "due:" ++ show date
  showColor (KVThreshold date) = bgColor Yellow $ color Blue $ "t:" ++ show date
  showColor (KVAt (TimeOfDay hr mn _)) = bgColor Blue $ "at:" ++ showPaddedNumber '0' 2 hr ++ showPaddedNumber '0' 2 mn
  showColor (KVString key value) = key ++ ":" ++ value


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

instance ShowColor StringTypes where
  showColor (SProject p) = color Blue $ "+" ++ p
  showColor (SContext c) = color Blue $ "@" ++ c
  showColor (SKeyValue kv) = showColor kv
  showColor (SOther s) = s

unrollStringTypes :: [StringTypes] -> String
unrollStringTypes = unwords . map show

unrollColorStringTypes :: [StringTypes] -> String
unrollColorStringTypes = unwords . map showColor

getProjects :: [StringTypes] -> [Project]
getProjects = foldr extractProjects []
  where
    extractProjects (SProject p) acc = p : acc
    extractProjects _ acc = acc

getContexts :: [StringTypes] -> [Context]
getContexts = foldr extractContexts []
  where
    extractContexts (SContext c) acc = c : acc
    extractContexts _ acc = acc

getKeyValues :: [StringTypes] -> [KeyValue]
getKeyValues = foldr extractKeyValues []
  where
    extractKeyValues (SKeyValue kv) acc = kv : acc
    extractKeyValues _ acc = acc

extractDueDate :: [StringTypes] -> Maybe Day
extractDueDate [] = Nothing
extractDueDate (SKeyValue (KVDueDate d):_) = Just d
extractDueDate (_:xs) = extractDueDate xs

extractThreshold :: [StringTypes] -> Maybe Day
extractThreshold [] = Nothing
extractThreshold (SKeyValue (KVThreshold d):_) = Just d
extractThreshold (_:xs) = extractThreshold xs

extractAt :: [StringTypes] -> Maybe TimeOfDay
extractAt [] = Nothing
extractAt (SKeyValue (KVAt tod):_) = Just tod
extractAt (_:xs) = extractAt xs

-- |Data type to store both incomplete and completed tasks.
data Task = Incomplete (Maybe Priority) (Maybe Day) [StringTypes]
          | Completed (Maybe Priority) (Maybe Day) (Maybe Day) [StringTypes]

-- |Show Task in format "(A) 2016-07-30 Task to do +Project @Context"
instance Show Task where
  show (Completed mPriority mEnd mStart sx) = "x "
                                         ++ showPriority mPriority
                                         ++ showDate mEnd
                                         ++ showDate mStart
                                         ++ unrollStringTypes sx
    where showPriority (Just p) = "(" ++ [p] ++ ") "
          showPriority Nothing = ""
          showDate (Just d) = show d ++ " "
          showDate Nothing = ""

  show (Incomplete mPriority mDate sx) = showPriority mPriority
                                                ++ showDate mDate
                                                ++ unrollStringTypes sx
    where showPriority (Just p) = "(" ++ [p] ++ ") "
          showPriority Nothing = ""
          showDate (Just d) = show d ++ " "
          showDate Nothing = ""

instance ShowColor Task where
  showColor (Completed mPriority mEnd mStart sx) = (color Red "x ")
                                                ++ showPriority mPriority
                                                ++ showDateRed mEnd
                                                ++ showDateGreen mStart
                                                ++ unrollColorStringTypes sx
    where showPriority (Just p) = color Magenta $ "(" ++ [p] ++ ") "
          showPriority Nothing = ""
          showDateRed (Just d) = color Red (show d ++ " ")
          showDateRed Nothing = ""
          showDateGreen (Just d) = color Green (show d ++ " ")
          showDateGreen Nothing = ""

  showColor (Incomplete mPriority mDate sx) = showPriority mPriority
                                                ++ showDateGreen mDate
                                                ++ unrollColorStringTypes sx
    where showPriority (Just p) = color Magenta $ "(" ++ [p] ++ ") "
          showPriority Nothing = ""
          showDateGreen (Just d) = color Green (show d ++ " ")
          showDateGreen Nothing = ""


-- |Comparisons are done based on priority
instance Eq Task where
  Incomplete{} == Completed{} = False
  Completed{} == Incomplete{} = False
  (Incomplete priA dateA _) == (Incomplete priB dateB _) =
    priA == priB && dateA == dateB
  (Completed priA endA startA _) == (Completed priB endB startB _) =
    priA == priB && endA == endB && startA == startB

-- |Comparisons are done based on priority.
instance Ord Task where
  -- Incomplete
  compare (Incomplete Nothing _ _) (Incomplete Nothing _ _) = EQ
  compare (Incomplete (Just _) _ _) (Incomplete Nothing _ _) = LT
  compare (Incomplete Nothing _ _) (Incomplete (Just _) _ _) = GT
  compare Incomplete{} Completed{} = LT
  compare Completed{} Incomplete{} = GT
  compare (Incomplete (Just a) _ _) (Incomplete (Just b) _ _) =
    compare a b

  -- Completed
  compare (Completed p1 e1 s1 _) (Completed p2 e2 s2 _) =
    comparePriority p1 p2 <> compareEndDate e1 e2 <> compareStartDate s1 s2


-- Helper functions for Ord
comparePriority :: Maybe Priority -> Maybe Priority -> Ordering
comparePriority Nothing Nothing = EQ
comparePriority Nothing _ = GT
comparePriority _ Nothing = LT
comparePriority (Just a) (Just b) = compare a b

compareEndDate :: Maybe Day -> Maybe Day -> Ordering
compareEndDate Nothing Nothing = EQ
compareEndDate Nothing _ = GT
compareEndDate _ Nothing = LT
compareEndDate (Just a) (Just b) = compare a b

compareStartDate :: Maybe Day -> Maybe Day -> Ordering
compareStartDate Nothing Nothing = EQ
compareStartDate Nothing _ = GT
compareStartDate _ Nothing = LT
compareStartDate (Just a) (Just b) = compare a b

-- |Filters out all completed tasks and returns a list of incomplete
isIncomplete :: Task -> Bool
isIncomplete Incomplete{} = True
isIncomplete _ = False

-- |Filters out all incomplete tasks and returns a list of completed
isCompleted :: Task -> Bool
isCompleted Completed{} = True
isCompleted _ = False

-- | Returns True if task contains the list of text
containsText :: [Text] -> Task -> Bool
containsText terms task = foldl (\res term -> res && T.toUpper term `T.isInfixOf` (T.toUpper . T.pack $ show task)) True terms

-- |Convert String to Date
convertToDate :: (AppError m, MonadDate m) => String -> m Day
convertToDate str
  | dayString == "today" = getDay
  | dayString == "tomorrow" = addDays 1 <$> getDay
  | dayString == "yesterday" = addDays (-1) <$> getDay
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
      if off <= wday
      then return . addDays 7 $ fromWeekDate yr wk off
      else return $ fromWeekDate yr wk off

-- |Convert String to Time
convertToTime :: (AppError m) => String -> m TimeOfDay
convertToTime str
  | timeString == "noon" = return midday
  | timeString == "midday" = return midday
  | timeString == "midnight" = return midnight
  | otherwise = throwError . EMiscError . T.pack $ "invalid time '" ++ str ++ "'"
  where
    timeString = map toLower str

-- |Convert all KV Due Dates that are strings into actual dates
convertStringType :: (AppError m, MonadDate m) => StringTypes -> m StringTypes
convertStringType (SKeyValue kv) =
    case kv of
      KVString "due" val -> do
        converted <- convertToDate val
        return . SKeyValue $ KVDueDate converted
      KVString "t" val -> do
        converted <- convertToDate val
        return . SKeyValue $ KVThreshold converted
      KVString "at" val -> do
        converted <- convertToTime val
        return . SKeyValue $ KVAt converted
      rest -> return $ SKeyValue rest
convertStringType rest = return rest

convertStringTypes :: (AppError m, MonadDate m) => [StringTypes] -> m [StringTypes]
convertStringTypes alst = forM alst convertStringType
