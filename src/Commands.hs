{-# LANGUAGE OverloadedStrings #-}
module Commands
    (
      processArgs
    , process
    , ConfigOption(..)
    ) where

import Control.Monad (forM_, msum)
import Data.Char (toUpper)
import Data.List (sort, sortBy, find, isInfixOf)
import Data.Maybe (isJust, fromJust)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath, replaceFileName)

import FileHandler (readTodoTxt, writeTodoTxt, appendTodoTxt, writeReportTxt)
import Parser (validateLine)
import RegEx (matchGen, swapGen, swapAllGen)
import Tasks ( Task(..)
             , Date(..)
             , StringTypes(..)
             , KeyValue(..)
             , Priority
             , onlyPending
             , onlyCompleted
             , convertStringTypes)
import Util (maybeRead)
import Version (version)

data ConfigOption = ConfigOption {
                                   todoTxtPath :: FilePath
                                 , archiveTxtPath :: Maybe FilePath
                                 , reportTxtPath :: Maybe FilePath
                                 , timeStamp :: Maybe Date
                                 }

-- |Defaults
defaultTodoName :: String
defaultTodoName = "todo.txt"

defaultArchiveName :: String
defaultArchiveName = "done.txt"

defaultReportName :: String
defaultReportName = "report.txt"

-- |Turn an array into a numbered array
numberify :: [a] -> [(Int, a)]
numberify = zipWith (\n t -> (n, t)) [1..]

-- |Turn a number array back into an array
denumbrify :: [(Int, a)] -> [a]
denumbrify = map (\(_,a) -> a)

-- |Print a numbered array
printTuple :: (Show a, Show b) => (a, b) -> IO ()
printTuple (n, t) = putStrLn $ show n ++ ": " ++ show t

-- |Returns the file path for todo.txt
-- Currently it is set to the home directory.
todoFilePath :: IO FilePath
todoFilePath = do
  home <- getHomeDirectory
  return $ joinPath [ home, defaultTodoName ]

-- |Extracts the archive file path for archive.txt
getArchivePath :: ConfigOption -> FilePath
getArchivePath cfg = case archiveTxtPath cfg of
                      Nothing -> replaceFileName (todoTxtPath cfg) defaultArchiveName
                      Just path -> path

-- |Extracts the report file path for report.txt
getReportPath :: ConfigOption -> FilePath
getReportPath cfg = case reportTxtPath cfg of
                      Nothing -> replaceFileName (todoTxtPath cfg) defaultReportName
                      Just path -> path

-- |Filter tasks based on Due Date.
-- Expects to be in a numbered tuple.
filterTupleDueDate :: [(Int, Task)] -> [(Int, Task)]
filterTupleDueDate = filter containsDueDate
  where
    isDue (SKeyValue (KVDueDate _)) = True
    isDue _ = False
    containsDueDate (_, (Incomplete _ _ sx)) = foldl (||) False $ map isDue sx
    containsDueDate (_, (Completed _ (Incomplete _ _ sx))) = foldl (||) False $ map isDue sx
    containsDueDate (_, (Completed _ (Completed _ _))) = error "Should never have a Completed Completed"

-- |Sort tuple based on Due Date
-- Expects to be in a numbered tuple.
sortTupleDueDate :: [(Int, Task)] -> [(Int, Task)]
sortTupleDueDate = sortBy sortFn
  where
    isDue (SKeyValue (KVDueDate _)) = True
    isDue _ = False
    getDueDate (Incomplete _ _ sx) = case find isDue sx of
                                      Just (SKeyValue (KVDueDate d)) -> Just d
                                      _ -> Nothing
    getDueDate _ = Nothing
    sortFn (_, x) (_, y) = compare (getDueDate x) (getDueDate y)

-- |Pass in command line arguments to be processed
processArgs :: [String] -> IO ()
processArgs args = do
  path <- todoFilePath
  process (ConfigOption path Nothing Nothing Nothing) args

-- |Reads the file, print error or calls a function to handle the list of todo
-- items.
process' :: FilePath -> ([Task] -> IO ()) -> IO ()
process' path  f = do
  todo <- readTodoTxt path
  case todo of
    Left e -> error (show e)
    Right xs -> do
      f xs

-- |Process command line arguments.
-- This function has multiple instances for performing the different actions.
process :: ConfigOption -> [String] -> IO ()

-- |Default Command, list all entries
-- Command Line:
process cfg [] = process' (todoTxtPath cfg) listAll
  where listAll = (\xss -> forM_ xss printTuple) . numberify
                                                 . sort
                                                 . onlyPending

-- |Flag for passing in the location of the todo.txt file
process cfg ("-t":path:rest) = process (cfg { todoTxtPath = path }) rest

-- |Flag for passing in the location of the archive.txt file
process cfg ("-a":path:rest) = process (cfg { archiveTxtPath = Just path }) rest

-- |Flag to auto start date all tasks
process cfg ("-s":rest) = do
  c <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay c
  process (cfg { timeStamp = Just (Date y m d) }) rest

-- |Flag for passing in the location of report.txt file
process cfg ("-r":path:rest) = process (cfg { reportTxtPath = Just path }) rest

-- |List entries with project and context filter
-- Command Line: list "string to match" +Project @Context
process cfg ("list":filters) = process' (todoTxtPath cfg) listSome
  where
    isInfixOfUpper s t = isInfixOf (map toUpper s) (map toUpper t)
    filterFn (_, t) = foldl (\b s -> b && (isInfixOfUpper s (show t))) True filters
    listSome = (\xss -> forM_ xss printTuple) . (filter filterFn)
                                              . numberify
                                              . sort
                                              . onlyPending

process cfg ("search":filters) = process' (todoTxtPath cfg) searchSome
  where
    matchFn = matchGen $ unwords filters
    filterFn (_, t) = matchFn $ show t
    searchSome = (\xss -> forM_ xss printTuple) . (filter filterFn)
                                                . numberify
                                                . sort
                                                . onlyPending

-- |Add task to list
-- Command Line: add Example Task for +Project with @Context
process cfg ("add":rest) = process' (todoTxtPath cfg) addToList
  where
    addToList oldLines = case validateLine (unwords rest) of
                            Left e -> error $ show e
                            Right newLine -> do
                              if isJust (timeStamp cfg)
                              then do
                                let Incomplete pri _ sx = newLine
                                sx' <- convertStringTypes sx
                                let newTask = Incomplete pri (timeStamp cfg) sx'
                                writeTodoTxt (todoTxtPath cfg) (oldLines ++ [newTask])
                                putStrLn $ "New Task: " ++ show newTask
                              else do
                                let Incomplete pri due sx = newLine
                                sx' <- convertStringTypes sx
                                let newTask = Incomplete pri due sx'
                                writeTodoTxt (todoTxtPath cfg) (oldLines ++ [newTask])
                                putStrLn $ "New Task: " ++ show newTask

-- |Delete task
-- Command Line: delete 1
process cfg ("delete":idx:[]) = process' (todoTxtPath cfg) deleteIdx
  where
    deleteIdx xs = do
      let nIdx = maybeRead idx :: Maybe Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (isJust nIdx) && (length xss >= (fromJust nIdx)) && ((fromJust nIdx) > 0)
      then do
        writeTodoTxt (todoTxtPath cfg)
                     $ (denumbrify $ filter (\(n, _) -> n /= (fromJust nIdx)) xss) ++ completed
        putStrLn "Task Deleted"
      else do
        error "Invalid Index: delete index"

-- |Mark Task Complete
-- Command Line: complete 1
process cfg ("complete":idx:[]) = process' (todoTxtPath cfg) completeIdx
  where
    completeIdx xs = do
      let nIdx = maybeRead idx :: Maybe Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (isJust nIdx) && (length xss >= (fromJust nIdx)) && ((fromJust nIdx) > 0)
      then do
        c <- getCurrentTime
        let (y, m, d) = toGregorian $ utctDay c
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= (fromJust nIdx)) xss
        let matches = denumbrify $ filter (\(n,_) -> n == (fromJust nIdx)) xss
        let complete = map (\t -> Completed (Date y m d) t) matches
        writeTodoTxt (todoTxtPath cfg) (nonMatch ++ complete ++ completed)
        putStrLn "Task Completed"
      else do
        error "Invalid Index: complete index"

-- |List only completed tasks
-- Command Line: completed
process cfg ("completed":[]) = process' (todoTxtPath cfg) completed
  where
    completed = (\xss -> forM_ xss printTuple) . numberify
                                               . sort
                                               . onlyCompleted

-- |Append to a currently existing task
-- Command Line: append 1 Text to add to task 1
process cfg ("append":idx:rest) = process' (todoTxtPath cfg) appendIdx
  where
    appendIdx xs = do
      let nIdx = maybeRead idx :: Maybe Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (isJust nIdx) && (length xss >= (fromJust nIdx)) && ((fromJust nIdx) > 0)
      then do
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= (fromJust nIdx)) xss
        let matches = denumbrify $ filter (\(n,_) -> n == (fromJust nIdx)) xss
        case matches of
          [m] -> do
            case validateLine (show m ++ " " ++ unwords rest) of
              Left e -> error $ show e
              Right updated -> do
                let Incomplete pri due sx = updated
                sx' <- convertStringTypes sx
                let updated' = Incomplete pri due sx'
                writeTodoTxt (todoTxtPath cfg) (nonMatch ++ [updated'] ++ completed)
                putStrLn $ "Updated Task: " ++ show updated'
          _ -> do error "Error in append"
      else do
        error "Invalid Index: append index \"text to append\""

-- |Prepend to a currently existing task
-- Command Line: prepend 1 Text to add to task 1
process cfg ("prepend":idx:rest) = process' (todoTxtPath cfg) prependIdx
  where
    prependIdx xs = do
      let nIdx = maybeRead idx :: Maybe Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (isJust nIdx) && (length xss >= (fromJust nIdx)) && ((fromJust nIdx) > 0)
      then do
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= (fromJust nIdx)) xss
        let matches = denumbrify $ filter (\(n,_) -> n == (fromJust nIdx)) xss
        case matches of
          [m] -> do
            case validateLine (unwords rest ++ " " ++ show m ++ " ") of
              Left e -> error $ show e
              Right updated -> do
                let Incomplete pri due sx = updated
                sx' <- convertStringTypes sx
                let updated' = Incomplete pri due sx'
                writeTodoTxt (todoTxtPath cfg) (nonMatch ++ [updated'] ++ completed)
                putStrLn $ "Updated Task: " ++ show updated'
          _ -> do error "Error in prepend"
      else do
        error "Invalid Index: prepend index \"text to prepend\""

-- |Replace existing task with text
-- Command Line: replace 1 "New text for task 1"
process cfg ("replace":idx:rest) = process' (todoTxtPath cfg) replaceIdx
  where
    replaceIdx xs = do
      let nIdx = maybeRead idx :: Maybe Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (isJust nIdx) && (length xss >= (fromJust nIdx)) && ((fromJust nIdx) > 0)
      then do
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= (fromJust nIdx)) xss
        let matches = denumbrify $ filter (\(n,_) -> n == (fromJust nIdx)) xss
        case matches of
          [m] -> do
            case validateLine (unwords rest) of
              Left e -> error $ show e
              Right updated -> do
                let Incomplete pri dt sx = updated
                let Incomplete pri' dt' _ = m
                let dt'' = timeStamp cfg
                sx' <- convertStringTypes sx
                let updated' = Incomplete (msum [pri, pri']) (msum [dt,dt',dt'']) sx'
                writeTodoTxt (todoTxtPath cfg) (nonMatch ++ [updated'] ++ completed)
                putStrLn $ "Updated Task: " ++ show updated'
          _ -> do error "Error in replace"
      else do
        error "Invalid Index: replace index \"text to replace\""

-- |Modifies the priority of a previously existing task
-- Command Line: priorty 1 B
process cfg ("priority":idx:priority:[]) = process' (todoTxtPath cfg) go
  where
    isPriority char = (char >= 'A' && char <= 'Z')
    go xs = do
      let nIdx = maybeRead idx :: Maybe Int
      let nPri = toUpper $ head priority
      if isPriority nPri
      then do
        updatePriority (todoTxtPath cfg) nIdx (Just nPri) xs
      else do
        error "Invalid Priority: Valid values A-Z or left blank for no priority"

process cfg ("priority":idx:[]) = process' (todoTxtPath cfg) go
  where
    go xs = do
      let nIdx = maybeRead idx :: Maybe Int
      updatePriority (todoTxtPath cfg) nIdx Nothing xs

process cfg ("due":[]) = process' (todoTxtPath cfg) listSome
  where
    listSome = (\xss -> forM_ xss printTuple)
             . sortTupleDueDate
             . filterTupleDueDate
             . numberify
             . sort
             . onlyPending

process cfg ("archive":[]) = process' (todoTxtPath cfg) archiveSome
  where
    archiveSome lst = do
      let iTasks = onlyPending lst
      let cTasks = onlyCompleted lst
      let archPath = getArchivePath cfg
      appendTodoTxt archPath cTasks
      writeTodoTxt (todoTxtPath cfg) iTasks
      putStrLn "Completed Tasks Archived"

process cfg ("report":[]) = process' (todoTxtPath cfg) reportSome
  where
    reportSome lst = do
      process cfg ["archive"] -- Do Archive first
      let iTasksLen = length $ onlyPending lst
      let archPath = getArchivePath cfg
      process' archPath (\archLst -> do
        let cTasksLen = length archLst
        let reportPath = getReportPath cfg
        writeReportTxt reportPath iTasksLen cTasksLen
        putStrLn $ "Report Created: " ++ show iTasksLen ++ " " ++ show cTasksLen)

-- |Help output
-- Command Line: help
process _ ("help":_) = do
  putStrLn "Usage: todo [-t path] [-s] action [task_number] [task_description]"
  putStrLn "Flags:"
  putStrLn $ " -t path    Points to todo.txt, default is $HOME/" ++ defaultTodoName
  putStrLn $ " -a path    Points to archive file, default is $HOME/" ++ defaultArchiveName
  putStrLn " -s         Auto timestamp new tasks"
  putStrLn $ " -r path    Points to report file, default is $HOME/" ++ defaultReportName
  putStrLn ""
  putStrLn "Actions:"
  putStrLn " add \"Task I need to do +project @context\""
  putStrLn " list|ls +project @context"
  putStrLn " search \"regular expression\""
  putStrLn " delete|del|remove|rm TASKNUM"
  putStrLn " complete|done|do TASKNUM"
  putStrLn " completed"
  putStrLn " priority|pri TASKNUM NEWPRIORITY"
  putStrLn " version"
  putStrLn " append TASKNUM \"addition to task\""
  putStrLn " prepend TASKNUM \"prepend to task\""
  putStrLn " replace TASKNUM \"text to replace\""
  putStrLn " due"
  putStrLn " archive"
  putStrLn " help"
  putStrLn ""
  putStrLn "Key Value Supported:"
  putStrLn " due - YYYY-MM-DD or today, tomorrow, yesterday, monday..sunday"
  putStrLn " all other use defined key/value pairs viewed as text"


-- |Show Version
-- Command Lin: version
process _ ("version":_) = do
  putStrLn $ "Version: " ++ version

-- Aliases
process cfg ("remove":idx:[]) = process cfg ("delete":idx:[])
process cfg ("del":idx:[]) = process cfg ("delete":idx:[])
process cfg ("rm":idx:[]) = process cfg ("delete":idx:[])
process cfg ("done":idx:[]) = process cfg ("complete":idx:[])
process cfg ("do":idx:[]) = process cfg ("complete":idx:[])
process cfg ("ls":rest) = process cfg ("list":rest)
process cfg ("pri":rest) = process cfg ("priority":rest)
process cfg _ = process cfg ("help":[])

updatePriority :: FilePath -> Maybe Int -> Maybe Priority -> [Tasks.Task] -> IO ()
updatePriority path nIdx mPri xs = do
  let xss = numberify $ sort $ onlyPending xs
  let completed = onlyCompleted xs
  if (isJust nIdx) && (length xs >= (fromJust nIdx)) && ((fromJust nIdx) > 0)
  then do
    let nonMatch = denumbrify $ filter (\(n,_) -> n /= (fromJust nIdx)) xss
    let matches = denumbrify $ filter (\(n,_) -> n == (fromJust nIdx)) xss
    let updated = map (\(Incomplete _ d sx) ->
                          Incomplete mPri d sx) matches
    writeTodoTxt path (nonMatch ++ updated ++ completed)
    putStrLn "Updated Priority"
  else do
    error "Invalid Index: priority index [priority]"
