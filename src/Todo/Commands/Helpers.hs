{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Todo.Commands.Helpers (
    defaultTodoName,
    defaultArchiveName,
    defaultReportName,
    todoFilePath,
    printTuple,
    printPrefixedTuple,
    readFileSafe,
    getPendingTodo,
    getCompletedTodo,
    getArchivedTodo,
    getAllTodo,
    writeTodo,
    writeArchive,
    writeReport,
    convertTaskStrings,
    readIndex,
    queryAction,
    queryConfirm,
    getIndexTasks,
    getNotIndexTasks,
    splitIndexTasks,
    replacePending,
    replaceCompleted,
    filterTupleDueDate,
    filterTupleCompleteDate,
    shortCircuit,
    filterThreshold
  ) where

import qualified Control.Exception as E
import           Control.Monad (forM_, mplus)
import           Data.List (sort, isInfixOf, intercalate, sortBy)
import           Data.Maybe (maybe, fromJust)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Time.Calendar (Day(..), diffDays)
import           Data.Time.Clock(UTCTime(..))
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Prelude hiding (readFile, writeFile)
import           System.Directory (getHomeDirectory)
import           System.FilePath (joinPath, replaceFileName)
import           System.IO (hFlush, stdout)
import           Text.Color
import           Todo.App
import           Todo.Parser
import           Todo.Tasks
import           Todo.Util

-- | Defaults
defaultTodoName :: String
defaultTodoName = "todo.txt"

defaultArchiveName :: String
defaultArchiveName = "done.txt"

defaultReportName :: String
defaultReportName = "report.txt"

-- | Returns the file path for todo.txt
-- Currently it is set to the home directory.
todoFilePath :: IO FilePath
todoFilePath = do
  home <- getHomeDirectory
  return $ joinPath [ home, defaultTodoName ]

-- |Print a numbered array
printTuple :: (Show a, ShowColor a, MonadIO m) => Bool -> [(Int, a)] -> m ()
printTuple useColor lst = forM_ lst print'
  where width = fromIntegral $ digitCount $ maximum $ map fst lst
        print' (n, t) = liftIO . putStrLn $ showPaddedNumber ' ' width n ++ ": " ++ (if useColor then showColor t else show t)

-- | Print tasks with prefixed message
printPrefixedTuple :: (Show a, MonadIO m) => Text -> [(Int, a)] -> m ()
printPrefixedTuple msg lst = forM_ lst print'
  where print' (_, t) = liftIO . putStrLn $ T.unpack msg <> ": " <> show t

-- | Reads a file and throws an error in the AppError context
readFileSafe :: (AppError m, MonadFileSystem m, MonadIO m) => Text -> m Text
readFileSafe path =
  either (throwError . EIOError) return =<< liftIO (E.try $ readFile path)

-- | Write a file and throws an error in the AppError context
writeFileSafe :: (AppError m, MonadFileSystem m, MonadIO m) => Text -> Text -> m ()
writeFileSafe path txt =
  either (throwError. EIOError) return =<< liftIO (E.try $ writeFile path txt)

-- | Append a file and thrws an error in the AppError context
appendFileSafe :: (AppError m, MonadFileSystem m, MonadIO m) => Text -> Text -> m ()
appendFileSafe path txt =
  readFileSafe path >>=
  (\oldTxt -> return . T.intercalate "\n" $ T.lines oldTxt <> T.lines txt)
  >>= writeFileSafe path

-- | Read only pending tasks from todo.txt, sorted and numbered.
getPendingTodo :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => m [(Int, Task)]
getPendingTodo = do
  path <- T.pack . todoTxtPath <$> get
  tasks <- readFileSafe path >>=
              either (throwError . EParseError) return . parseLines (T.unpack path) . T.unpack
  let pending = sort $ filter isIncomplete tasks
  return $ zip [1..] pending

-- | Read only completed tasks from todo.txt, sorted and numbered.
getCompletedTodo :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => m [(Int, Task)]
getCompletedTodo = do
  path <- T.pack . todoTxtPath <$> get
  tasks <- readFileSafe path >>=
              either (throwError . EParseError) return . parseLines (T.unpack path) . T.unpack
  let completed = sort $ filter isCompleted tasks
  return $ zip [1..] completed

-- | Read all tasks from todo.txt, sorted and numbered.
getAllTodo :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => m [(Int, Task)]
getAllTodo = do
  path <- T.pack . todoTxtPath <$> get
  tasks <- readFileSafe path >>=
              either (throwError . EParseError) return . parseLines (T.unpack path) . T.unpack
  let all = sort tasks
  return $ zip [1..] all

-- | Read all tasks from archive file, sorted and numbered.
getArchivedTodo :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => m [(Int, Task)]
getArchivedTodo = do
  archivePath <- archiveTxtPath <$> get
  todoPath <- todoTxtPath <$> get
  let path = fromJust $ mplus archivePath (Just $ replaceFileName todoPath defaultArchiveName)
  tasks <- readFileSafe (T.pack path) >>=
              either (throwError . EParseError) return . parseLines path . T.unpack
  let sorted = sort tasks
  return $ zip [1..] sorted

-- | Writes list of todo, overriding old file
writeTodo :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => [(Int, Task)] -> m ()
writeTodo numberedTasks = do
  path <- T.pack . todoTxtPath <$> get
  writeFileSafe path (T.intercalate "\n" . map (T.pack . show . snd) $ sortBy (\(_,t1) (_,t2) -> compare t1 t2) numberedTasks)

-- | Writes list of todo to archive, appending old file
writeArchive :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => [(Int, Task)] -> m ()
writeArchive numberedTasks = do
  archivePath <- archiveTxtPath <$> get
  todoPath <- todoTxtPath <$> get
  let path = fromJust $ mplus archivePath (Just $ replaceFileName todoPath defaultArchiveName)
  appendFileSafe (T.pack path) (T.intercalate "\n" . map (T.pack . show . snd) $ sortBy (\(_,t1) (_,t2) -> compare t1 t2) numberedTasks)

-- writeReport :: (AppError m, AppConfig m, MonadDate m, MonadFileSystem m, MonadIO m) => Int -> Int -> m ()
writeReport pending archived = do
  reportPath <- reportTxtPath <$> get
  todoPath <- todoTxtPath <$> get
  let path = fromJust $ mplus reportPath (Just $ replaceFileName todoPath defaultReportName)
  liftIO $ putStrLn $ "Writing to " ++ show path
  now <- getUTCTime
  appendFileSafe (T.pack path) $ T.pack $ iso8601 now ++ " " ++ show pending ++ " " ++ show archived

-- | Converts all string types of task
convertTaskStrings :: (AppConfig m, AppError m, MonadDate m) => Task -> m Task
convertTaskStrings (Incomplete pri now sx) = do
  sx' <- convertStringTypes sx
  st <- get
  let stamp = timeStamp st
  let time = stamp `mplus` now
  return $ Incomplete pri time sx'

-- | Read index or throw an error
readIndex :: (AppError m) => Text -> m Int
readIndex index = maybe (throwError $ EInvalidArg index) return (maybeRead (T.unpack index) :: Maybe Int)

-- | Queries the user, yes or no
queryAction :: (AppConfig m, MonadIO m) => [Task] -> Text -> m Bool
queryAction tasks str =
  autoAccept <$> get >>=
  maybe (queryAction' tasks str) return

queryAction' :: (MonadIO m) => [Task] -> Text -> m Bool
queryAction' tasks str =
    mapM_ (liftIO . print) tasks >>
    (liftIO . T.putStr $ str <> " (N/y)? ") >>
    liftIO (hFlush stdout) >>
    liftIO readChar >>= f
  where f 'Y' = liftIO (putStrLn "") >> return True
        f 'y' = liftIO (putStrLn "") >> return True
        f '\n' = return False  -- Special case because it prints a new line
        f _ = liftIO (putStrLn "") >> return False

-- | Query the user, yes or no, based on args
queryConfirm :: (AppConfig m, MonadIO m) => [Task] -> Text -> m Bool
queryConfirm tasks str = do
  force <- forcedPrompt <$> get
  if force
  then queryAction tasks str -- Query the task
  else return True -- Do the task

getIndexTasks :: (AppError m) => Int -> [(Int, Task)] -> m [(Int, Task)]
getIndexTasks index tasks = maybe (throwError $ EInvalidIndex index) return $ maybeFilter ((== index) . fst) tasks

getNotIndexTasks :: (AppError m) => Int -> [(Int, Task)] -> m [(Int, Task)]
getNotIndexTasks index tasks = maybe (throwError $ EInvalidIndex index) return $ maybeFilter ((/= index) . fst) tasks

splitIndexTasks :: (AppError m) => [Int] -> [(Int, Task)] -> m ([(Int, Task)], [(Int, Task)])
splitIndexTasks indexes tasks = do
  let matches = filter ((`elem` indexes) . fst) tasks
  let nonMatches = filter ((`notElem` indexes) . fst) tasks
  if length matches == length indexes
  then return (matches, nonMatches)
  else throwError $ EInvalidIndexes $ filter (`notElem` map fst matches) indexes

replacePending :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => [(Int, Task)] -> m ()
replacePending newPending =
  writeTodo =<< (newPending <>) <$> getCompletedTodo

replaceCompleted :: (AppError m, AppConfig m, MonadFileSystem m, MonadIO m) => [(Int, Task)] -> m()
replaceCompleted newComplete =
  writeTodo =<< (newComplete <>) <$> getPendingTodo

filterTupleDueDate :: Day -> [(Int, Task)] -> [(Int, Task)]
filterTupleDueDate dueDate = filter containsDueDate
  where
    isDue (SKeyValue (KVDueDate d)) = d <= dueDate
    isDue _ = False
    containsDueDate (_, Incomplete _ _ sx) = any isDue sx
    containsDueDate (_, Completed _ (Incomplete _ _ sx)) = any isDue sx
    containsDueDate (_, Completed _ (Completed _ _)) = False

filterTupleCompleteDate :: Day -> [(Int, Task)] -> [(Int, Task)]
filterTupleCompleteDate completeDate = filter containsCompleteDate
  where
    containsCompleteDate (_, Incomplete{}) = False
    containsCompleteDate (_, Completed d _) = d == completeDate

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

shortCircuit :: (AppError m) => Text -> m ()
shortCircuit = throwError . EShortCircuit

filterThreshold :: (MonadDate m, MonadIO m) => [(Int, Task)] -> m [(Int, Task)]
filterThreshold lst = do
  now <- getDay
  return $ filter (\(i, t) -> case t of
                                Completed _ _ -> True
                                Incomplete _ _ str -> maybe True (<= now) (extractThreshold str)) lst
