{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Todo.Commands.Helpers (
    defaultTodoName,
    defaultArchiveName,
    defaultReportName,
    todoFilePath,
    printShowable,
    printTuple,
    printPrefixedTuple,
    readFileMErr,
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
    filterThreshold,
    filterTuplePriority
  ) where

import           Control.Monad (forM_, mplus)
import           Data.List (sort, sortBy)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Time.Calendar (Day(..))
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

-- | Print Options
printShowable :: (Show a, MonadIO m) => a -> m ()
printShowable op = liftIO . putStrLn $ show op

-- |Print a numbered array
printTuple :: (ShowColor a, MonadIO m) => Bool -> [(Int, a)] -> m ()
printTuple useColor lst = forM_ lst print'
  where width = fromIntegral $ digitCount $ maximum $ map fst lst
        print' (n, t) = liftIO . putStrLn $ showPaddedNumber ' ' width n ++ ": " ++ (if useColor then showColor t else show t)

-- | Print tasks with prefixed message
printPrefixedTuple :: (Show a, MonadIO m) => Text -> [(Int, a)] -> m ()
printPrefixedTuple msg lst = forM_ lst print'
  where print' (_, t) = liftIO . putStrLn $ T.unpack msg <> ": " <> show t

-- | Reads a file and throws an error in the AppError context
readFileMErr :: (AppError m, MonadFileSystem m) => Text -> m Text
readFileMErr path = do
    result <- readFileSafe path
    case result of
      Left ioErr -> throwError $ EIOError ioErr
      Right content -> return content

-- | Write a file and throws an error in the AppError context
writeFileMErr :: (AppError m, MonadFileSystem m) => Text -> Text -> m ()
writeFileMErr path txt = do
  result <- writeFileSafe path txt
  case result of
    Left ioErr -> throwError $ EIOError ioErr
    Right () -> return ()

-- | Append a file and thrws an error in the AppError context
appendFileMErr :: (AppError m, MonadFileSystem m) => Text -> Text -> m ()
appendFileMErr path txt =
  readFileMErr path >>=
  (\oldTxt -> return . T.intercalate "\n" $ T.lines oldTxt <> T.lines txt)
  >>= writeFileMErr path

-- | Read only pending tasks from todo.txt, sorted and numbered.
getPendingTodo :: (AppError m, AppConfig m, MonadFileSystem m) => m [(Int, Task)]
getPendingTodo = do
  path <- T.pack . todoTxtPath <$> get
  tsks <- readFileMErr path >>=
              either (throwError . EParseError) return . parseLines (T.unpack path) . T.unpack
  let pending = sort $ filter isIncomplete tsks
  return $ zip [1..] pending

-- | Read only completed tasks from todo.txt, sorted and numbered.
getCompletedTodo :: (AppError m, AppConfig m, MonadFileSystem m) => m [(Int, Task)]
getCompletedTodo = do
  path <- T.pack . todoTxtPath <$> get
  tsks <- readFileMErr path >>=
              either (throwError . EParseError) return . parseLines (T.unpack path) . T.unpack
  let completed = sort $ filter isCompleted tsks
  return $ zip [1..] completed

-- | Read all tasks from todo.txt, sorted and numbered.
getAllTodo :: (AppError m, AppConfig m, MonadFileSystem m) => m [(Int, Task)]
getAllTodo = do
  path <- T.pack . todoTxtPath <$> get
  tsks <- readFileMErr path >>=
              either (throwError . EParseError) return . parseLines (T.unpack path) . T.unpack
  let allTasks = sort tsks
  return $ zip [1..] allTasks

-- | Read all tasks from archive file, sorted and numbered.
getArchivedTodo :: (AppError m, AppConfig m, MonadFileSystem m) => m [(Int, Task)]
getArchivedTodo = do
  archivePath <- archiveTxtPath <$> get
  todoPath <- todoTxtPath <$> get
  let path = fromJust $ mplus archivePath (Just $ replaceFileName todoPath defaultArchiveName)
  tsks <- readFileMErr (T.pack path) >>=
              either (throwError . EParseError) return . parseLines path . T.unpack
  let sorted = sort tsks
  return $ zip [1..] sorted

-- | Writes list of todo, overriding old file
writeTodo :: (AppError m, AppConfig m, MonadFileSystem m) => [(Int, Task)] -> m ()
writeTodo numberedTasks = do
  path <- T.pack . todoTxtPath <$> get
  writeFileMErr path (T.intercalate "\n" . map (T.pack . show . snd) $ sortBy (\(_,t1) (_,t2) -> compare t1 t2) numberedTasks)

-- | Writes list of todo to archive, appending old file
writeArchive :: (AppError m, AppConfig m, MonadFileSystem m) => [(Int, Task)] -> m ()
writeArchive numberedTasks = do
  archivePath <- archiveTxtPath <$> get
  todoPath <- todoTxtPath <$> get
  let path = fromJust $ mplus archivePath (Just $ replaceFileName todoPath defaultArchiveName)
  appendFileMErr (T.pack path) (T.intercalate "\n" . map (T.pack . show . snd) $ sortBy (\(_,t1) (_,t2) -> compare t1 t2) numberedTasks)

writeReport :: (AppError m, AppConfig m, MonadDate m, MonadFileSystem m, MonadIO m) => Int -> Int -> m ()
writeReport pending archived = do
  reportPath <- reportTxtPath <$> get
  todoPath <- todoTxtPath <$> get
  let path = fromJust $ mplus reportPath (Just $ replaceFileName todoPath defaultReportName)
  liftIO $ putStrLn $ "Writing to " ++ show path
  now <- getUTCTime
  appendFileMErr (T.pack path) $ T.pack $ iso8601 now ++ " " ++ show pending ++ " " ++ show archived

-- | Converts all string types of task
convertTaskStrings :: (AppConfig m, AppError m, MonadDate m) => Task -> m Task
convertTaskStrings (Incomplete pri now sx) = do
  sx' <- convertStringTypes sx
  st <- get
  let stamp = timeStamp st
  let time = stamp `mplus` now
  return $ Incomplete pri time sx'
convertTaskStrings (Completed d (Incomplete pri now sx)) = do
  sx' <- convertStringTypes sx
  st <- get
  let stamp = timeStamp st
  let time = stamp `mplus` now
  return $ Completed d (Incomplete pri time sx')
convertTaskStrings _ = throwError $ EMiscError "Invalid task type for conversion"

-- | Read index or throw an error
readIndex :: (AppError m) => Text -> m Int
readIndex index = maybe (throwError $ EInvalidArg index) return (maybeRead (T.unpack index) :: Maybe Int)

-- | Queries the user, yes or no
queryAction :: (AppConfig m, MonadIO m) => [Task] -> Text -> m Bool
queryAction tsks str =
  (prettyPrinting <$> get) >>= \pretty ->
      autoAccept <$> get >>=
      maybe (queryAction' pretty tsks str) return

queryAction' :: (MonadIO m) => Bool -> [Task] -> Text -> m Bool
queryAction' useColor tsks str =
    mapM_ (liftIO . putStrLn . (if useColor then showColor else show)) tsks >>
    (liftIO . T.putStr $ str <> " (N/y)? ") >>
    liftIO (hFlush stdout) >>
    liftIO readChar >>= f
  where f 'Y' = liftIO (putStrLn "") >> return True
        f 'y' = liftIO (putStrLn "") >> return True
        f '\n' = return False  -- Special case because it prints a new line
        f _ = liftIO (putStrLn "") >> return False

-- | Query the user, yes or no, based on args
queryConfirm :: (AppConfig m, MonadIO m) => [Task] -> Text -> m Bool
queryConfirm tsks str = do
  force <- forcedPrompt <$> get
  if force
  then queryAction tsks str -- Query the task
  else return True -- Do the task

getIndexTasks :: (AppError m) => Int -> [(Int, Task)] -> m [(Int, Task)]
getIndexTasks index tsks = maybe (throwError $ EInvalidIndex index) return $ maybeFilter ((== index) . fst) tsks

getNotIndexTasks :: (AppError m) => Int -> [(Int, Task)] -> m [(Int, Task)]
getNotIndexTasks index tsks = maybe (throwError $ EInvalidIndex index) return $ maybeFilter ((/= index) . fst) tsks

splitIndexTasks :: (AppError m) => [Int] -> [(Int, Task)] -> m ([(Int, Task)], [(Int, Task)])
splitIndexTasks indexes tsks = do
  let matches = filter ((`elem` indexes) . fst) tsks
  let nonMatches = filter ((`notElem` indexes) . fst) tsks
  if length matches == length indexes
  then return (matches, nonMatches)
  else throwError $ EInvalidIndexes $ filter (`notElem` map fst matches) indexes

replacePending :: (AppError m, AppConfig m, MonadFileSystem m) => [(Int, Task)] -> m ()
replacePending newPending =
  writeTodo =<< (newPending <>) <$> getCompletedTodo

replaceCompleted :: (AppError m, AppConfig m, MonadFileSystem m) => [(Int, Task)] -> m()
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

filterThreshold :: (MonadDate m) => [(Int, Task)] -> m [(Int, Task)]
filterThreshold lst = do
  now <- getDay
  return $ filter (\(_, t) -> case t of
                                Completed _ _ -> True
                                Incomplete _ _ str -> maybe True (<= now) (extractThreshold str)) lst

filterTuplePriority :: Priority -> [(Int, Task)] -> [(Int, Task)]
filterTuplePriority pri = filter priOrHigher
  where
    isHigherPriority (Incomplete (Just p) _ _) = p <= pri
    isHigherPriority _ = False
    priOrHigher (_, tsk) = isHigherPriority tsk
