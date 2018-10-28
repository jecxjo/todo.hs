module Todo.Parser
    (
      whiteSpace
    , priority
    , date
    , project
    , context
    , keyvalue
    , other
    , stringTypes
    , lots
    , incompleteTask
    , completedTask
    , task
    , tasks
    , Todo.Parser.ParseError
    , parseLines
    , validateLine
    ) where

import Control.Applicative (many)
import Control.Monad (liftM)
import Data.Char (toUpper)
import Data.List (concat)
import Data.Maybe (isJust, fromJust)
import Data.Text (pack)
import Data.Time.Calendar (fromGregorianValid, Day(..))
import Data.Time.LocalTime (TimeOfDay(..), makeTimeOfDayValid)
import Text.Parsec.Char ( char
                        , oneOf
                        , letter
                        , alphaNum
                        , digit
                        , noneOf
                        , endOfLine
                        , string)
import Text.Parsec.Combinator ( many1
                              , optionMaybe
                              , choice
                              , option)
import Text.Parsec.Error as E
import Text.Parsec.Prim (parse, try, parserFail)
import Text.Parsec.Text

import qualified Todo.Tasks as Tasks

type ParseError = E.ParseError

--
-- Atoms
--

-- |Parse and ignore all white space
whiteSpace :: Parser ()
whiteSpace = () <$ many (char ' ')

-- |Priority: (A)
priority :: Parser Tasks.Priority
priority = do
  _ <- char '('
  p <- letter
  _ <- char ')'
  return $ toUpper p

-- |Date: 2017-02-23
-- Supports 2 or 4 digit year, and 1 or 2 digit month and day.
date :: Parser Day
date = do
    year <- liftM (convertYear . read) twoToFourDigits
    _ <- char '-'
    month <- liftM read oneToTwoDigits
    _ <- char '-'
    day <- liftM read oneToTwoDigits
    let maybeDay = fromGregorianValid year month day
    if (isJust maybeDay)
      then return $ fromJust maybeDay
      else parserFail $ "Invalid Date: " ++ show year ++ "-" ++ show month ++ "-" ++ show day
  where oneToTwoDigits = do
          x <- digit
          y <- option ' ' $ digit
          return (x:y:[])
        twoToFourDigits = do
          w <- digit
          x <- digit
          y <- option ' ' $ digit
          z <- option ' ' $ digit
          return (w:x:y:z:[])
        convertYear x = if x < 100
                        then x + 2000
                        else x

-- |Project: +ProjectName +Project.SubProject +Project-Sub-SubSub
project :: Parser Tasks.StringTypes
project = do
    _ <- char '+'
    p <- many1 snakeCase
    s <- many subProjects
    _ <- whiteSpace
    return $ Tasks.SProject (concat $ p : s)
  where
    snakeCase = choice [alphaNum, char '_']
    subProjects = do
      x <- oneOf "-."
      sub <- many1 snakeCase
      return $ x : sub

-- |Context: @ContextString
context :: Parser Tasks.StringTypes
context = do
    _ <- char '@'
    s <- many alphaNumDashDotPlusAmpUnder
    _ <- whiteSpace
    return $ Tasks.SContext s
  where
    alphaNumDashDotPlusAmpUnder = choice [alphaNum, oneOf "-.@+_"]

-- |Key Value Pair: key:value
kvstring :: Parser Tasks.StringTypes
kvstring = try $ do
  key <- many1 alphaNum
  _ <- char ':'
  value <- many1 $ choice [alphaNum, oneOf "`~!@#$%^&*()_+-=[]\\{}|;',./<>?\""]
  _ <- whiteSpace
  return . Tasks.SKeyValue $ Tasks.KVString key value

kvduedate :: Parser Tasks.StringTypes
kvduedate = try $ do
  _ <- string "due:"
  d <- date
  _ <- whiteSpace
  return . Tasks.SKeyValue $ Tasks.KVDueDate d

kvthreshold :: Parser Tasks.StringTypes
kvthreshold = try $ do
  _ <- string "t:"
  d <- date
  _ <- whiteSpace
  return . Tasks.SKeyValue $ Tasks.KVThreshold d

kvat :: Parser Tasks.StringTypes
kvat = try $ do
    _ <- string "at:"
    hr <- twoDigit
    min <- twoDigit
    _ <- whiteSpace
    let tod = makeTimeOfDayValid hr min 0
    if (isJust tod) then
      return . Tasks.SKeyValue . Tasks.KVAt $ fromJust tod
    else
      fail "Not a valid time"
  where
    twoDigit = do
      x <- digit
      y <- digit
      return $ read (x:y:[])

keyvalue :: Parser Tasks.StringTypes
keyvalue = choice [ kvduedate, kvthreshold, kvat, kvstring ]

-- |Other string content
-- This parser removes any spacess and newlines from beginning.
other :: Parser Tasks.StringTypes
other = do
  cx <- many1 $ noneOf "\n "
  _ <- whiteSpace
  return $ Tasks.SOther cx

-- |Parse all string types
-- Order is important here. Since Project and Context start with a special
-- character which can be found inside any other string, they must be the
-- first choices and other as the fall back.
stringTypes :: Parser Tasks.StringTypes
stringTypes = choice [
                       project
                     , context
                     , keyvalue
                     , other
                     ]

-- |Parse a lot of string types
lots :: Parser [Tasks.StringTypes]
lots = do
  l <- many1 stringTypes
  return l

--
-- Full Task strings
--

-- |Incomplete Task
-- This supports an optional priority, and an optional start date.
incompleteTask :: Parser Tasks.Task
incompleteTask = do
  pri <- optionMaybe priority
  _ <- whiteSpace
  startDate <- optionMaybe date
  _ <- whiteSpace
  rest <- lots
  _ <- many endOfLine
  return $ Tasks.Incomplete pri startDate rest

-- |Complete Task
-- It is assumed that a completed task starts with x and a required completion
-- date. It is also assumed that the rest of the string will be an incomplete
-- task.
completedTask :: Parser Tasks.Task
completedTask = do
  _ <- char 'x'
  _ <- whiteSpace
  endDate <- date
  _ <- whiteSpace
  t <- incompleteTask
  _ <- many endOfLine
  return $ Tasks.Completed endDate t

-- |Either Incomplete or Completed Tasks
-- When parsing the todo.txt file the order of entries is not defined. There
-- could be Incomplete and Completed tasks throughout the file. Order in the
-- choice call is set as Completed first because the contents could parse into
-- an Incomplete due to the optional atoms.
task :: Parser Tasks.Task
task = do
  _ <- try (many (char '\n'))
  t <- choice [ completedTask, incompleteTask ]
  return t

tasks :: Parser [Tasks.Task]
tasks = do
  tx <- option [] . try $ many task
  return tx

-- |Parses entire lines. This call includes the string for the file path which
-- is used in the error message.
parseLines :: String -> String -> Either Todo.Parser.ParseError [Tasks.Task]
parseLines path lns = parse tasks path (pack lns)

-- |Validates a single line. This is used to check if the string passed for
-- creating a new task is valid.
validateLine :: String -> Either Todo.Parser.ParseError Tasks.Task
validateLine content = parse task "" (pack content)
