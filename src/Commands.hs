{-# LANGUAGE OverloadedStrings #-}
module Commands
    (
      processArgs
    , process
    , ConfigOption(..)
    ) where

import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.List (sort, isPrefixOf, sortBy, find, isInfixOf)
import Data.Maybe (isJust)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Data.Time.Calendar (Day(..))
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

import FileHandler (readTodoTxt, writeTodoTxt)
import Parser (validateLine)
import Tasks ( Task(..)
             , Date(..)
             , StringTypes(..)
             , KeyValue(..)
             , Priority
             , Project
             , Context
             , onlyPending
             , onlyCompleted
             , getProjects
             , getContexts
             , convertStringTypes)
import Util (subsetOf, MonadDate(..))
import Version (version)

instance MonadDate IO where
  getDay = do
    c <- getCurrentTime
    return $ utctDay c

data ConfigOption = ConfigOption {
                                   todoTxtPath :: FilePath
                                 , timeStamp :: Maybe Date
                                 }

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
  return $ joinPath [ home, "todo.txt" ]

-- |Filter tasks based on Projects.
-- Expects to be in a numbered tuple.
filterTupleProjects :: [Project] -> [(Int, Task)] -> [(Int, Task)]
filterTupleProjects px = filter projectFilter
  where
    projectFilter (_, (Incomplete _ _ sx)) = px `subsetOf` (getProjects sx)
    projectFilter (_, (Completed _ (Incomplete _ _ sx))) =
      px `subsetOf` (getProjects sx)

-- |Filter tasks based on Context.
-- Expects to be in a numbered tuple.
filterTupleContexts :: [Context] -> [(Int, Task)] -> [(Int, Task)]
filterTupleContexts cx = filter contextFilter
  where
    contextFilter (_, (Incomplete _ _ sx)) = cx `subsetOf` (getContexts sx)
    contextFilter (_, (Completed _ (Incomplete _ _ sx))) =
      cx `subsetOf` (getContexts sx)

-- |Filter tasks based on Due Date.
-- Expects to be in a numbered tuple.
filterTupleDueDate :: [(Int, Task)] -> [(Int, Task)]
filterTupleDueDate = filter containsDueDate
  where
    isDue (SKeyValue (KVDueDate _)) = True
    isDue _ = False
    containsDueDate (_, (Incomplete _ _ sx)) = foldl (||) False $ map isDue sx
    containsDueDate (_, (Completed _ (Incomplete _ _ sx))) = foldl (||) False $ map isDue sx

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
  process (ConfigOption path Nothing) args

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

-- |Flag to auto start date all tasks
process cfg ("-s":rest) = do
  c <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay c
  process (cfg { timeStamp = Just (Date y m d) }) rest

-- |List entries with project and context filter
-- Command Line: list "string to match" +Project @Context
process cfg ("list":filters) = process' (todoTxtPath cfg) listSome
  where
    filterFn (_, t) = foldl (\b s -> b && (isInfixOf s (show t))) True filters
    listSome = (\xss -> forM_ xss printTuple) . (filter filterFn)
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
      let nIdx = read idx :: Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        writeTodoTxt (todoTxtPath cfg)
                     $ (denumbrify $ filter (\(n, _) -> n /= nIdx) xss) ++ completed
        putStrLn "Task Deleted"
      else do
        error "Invalid Index"

-- |Mark Task Complete
-- Command Line: complete 1
process cfg ("complete":idx:[]) = process' (todoTxtPath cfg) completeIdx
  where
    completeIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        c <- getCurrentTime
        let (y, m, d) = toGregorian $ utctDay c
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
        let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
        let complete = map (\t -> Completed (Date y m d) t) matches
        writeTodoTxt (todoTxtPath cfg) (nonMatch ++ complete ++ completed)
        putStrLn "Task Completed"
      else do
        error "Invalid Index"

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
      let nIdx = read idx :: Int
      let xss = numberify $ sort $ onlyPending xs
      let completed = onlyCompleted xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
        let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
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
        error "Invalid Index"

-- |Modifies the priority of a previously existing task
-- Command Line: priorty 1 B
process cfg ("priority":idx:priority:[]) = process' (todoTxtPath cfg) go
  where
    isPriority char = (char >= 'A' && char <= 'Z')
    go xs = do
      let nIdx = read idx :: Int
      let nPri = toUpper $ head priority
      if isPriority nPri
      then do
        updatePriority (todoTxtPath cfg) nIdx (Just nPri) xs
      else do
        error "Invalid Priority"

process cfg ("priority":idx:[]) = process' (todoTxtPath cfg) go
  where
    go xs = do
      let nIdx = read idx :: Int
      updatePriority (todoTxtPath cfg) nIdx Nothing xs

process cfg ("due":[]) = process' (todoTxtPath cfg) listSome
  where
    listSome = (\xss -> forM_ xss printTuple)
             . sortTupleDueDate
             . filterTupleDueDate
             . numberify
             . sort
             . onlyPending

-- |Help output
-- Command Line: help
process _ ("help":_) = do
  putStrLn "Usage: todo [-t path] [-s] action [task_number] [task_description]"
  putStrLn "Flags:"
  putStrLn " -t path    Points to todo.txt, default is $HOME/todo.txt"
  putStrLn " -s         Auto timestamp new tasks"
  putStrLn ""
  putStrLn "Actions:"
  putStrLn " add \"Task I need to do +project @context\""
  putStrLn " list|ls +project @context"
  putStrLn " del|delete|remove TASKNUM"
  putStrLn " complete|done TASKNUM"
  putStrLn " completed"
  putStrLn " priority|pri TASKNUM NEWPRIORITY"
  putStrLn " version"
  putStrLn " append TASKNUM \"addition to task\""
  putStrLn " due"
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
process cfg ("done":idx:[]) = process cfg ("complete":idx:[])
process cfg ("ls":rest) = process cfg ("list":rest)
process cfg ("pri":rest) = process cfg ("priority":rest)
process cfg _ = process cfg ("help":[])

updatePriority :: FilePath -> Int -> Maybe Priority -> [Tasks.Task] -> IO ()
updatePriority path nIdx mPri xs = do
  let xss = numberify $ reverse $ sort $ onlyPending xs
  let completed = onlyCompleted xs
  if (length xs >= nIdx) && (nIdx > 0)
  then do
    let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
    let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
    let updated = map (\(Incomplete _ d sx) ->
                          Incomplete mPri d sx) matches
    writeTodoTxt path (nonMatch ++ updated ++ completed)
    putStrLn "Updated Priority"
  else do
    error "Invalid Index"

