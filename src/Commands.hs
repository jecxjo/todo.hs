{-# LANGUAGE OverloadedStrings #-}
module Commands
    (
      processArgs
    , process
    , ConfigOption(..)
    ) where

import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.List (sort, isPrefixOf)
import Data.Maybe (isJust)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

import FileHandler (readTodoTxt, writeTodoTxt)
import Parser (validateLine)
import Tasks ( Task(..)
             , Date(..)
             , Priority
             , Project
             , Context
             , onlyPending
             , onlyCompleted
             , filterProjects
             , filterContext)
import Util (subsetOf)
import Version (version)

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
    projectFilter (_, (Incomplete _ _ projs _ _)) = px `subsetOf` projs
    projectFilter (_, (Completed _ (Incomplete _ _ projs _ _))) =
      px `subsetOf` projs

-- |Filter tasks based on Context.
-- Expects to be in a numbered tuple.
filterTupleContexts :: [Context] -> [(Int, Task)] -> [(Int, Task)]
filterTupleContexts cx = filter contextFilter
  where
    contextFilter (_, (Incomplete _ _ _ ctx _)) = cx `subsetOf` ctx
    contextFilter (_, (Completed _ (Incomplete _ _ _ ctx _))) =
      cx `subsetOf` ctx

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
                                                 . reverse
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
-- Command Line: list +Project @Context
process cfg ("list":filters) = process' (todoTxtPath cfg) listSome
  where
    projects = map (\f -> drop 1 f) $ filter (\f -> isPrefixOf "+" f) filters
    contexts = map (\f -> drop 1 f) $ filter (\f -> isPrefixOf "@" f) filters
    fP = filterTupleProjects projects
    fC = filterTupleContexts contexts
    listSome = (\xss -> forM_ xss printTuple) . fC
                                              . fP
                                              . numberify
                                              . reverse
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
                                let Incomplete pri _ pro con str = newLine
                                let newTask = Incomplete pri (timeStamp cfg) pro con str
                                writeTodoTxt (todoTxtPath cfg) (newTask:oldLines)
                                putStrLn $ "New Task: " ++ show newTask
                              else do
                                writeTodoTxt (todoTxtPath cfg) (newLine:oldLines)
                                putStrLn $ "New Task: " ++ show newLine

-- |Delete task
-- Command Line: delete 1
process cfg ("delete":idx:[]) = process' (todoTxtPath cfg) deleteIdx
  where
    deleteIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ reverse $ sort $ onlyPending xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        writeTodoTxt (todoTxtPath cfg)
                     $ denumbrify $ filter (\(n, _) -> n /= nIdx) xss
        putStrLn "Task Deleted"
      else do
        error "Invalid Index"

-- |Mark Task Complete
-- Command Line: complete 1
process cfg ("complete":idx:[]) = process' (todoTxtPath cfg) completeIdx
  where
    completeIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ reverse $ sort $ onlyPending xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        c <- getCurrentTime
        let (y, m, d) = toGregorian $ utctDay c
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
        let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
        let completed = map (\t -> Completed (Date y m d) t) matches
        writeTodoTxt (todoTxtPath cfg) (nonMatch ++ completed)
        putStrLn "Task Completed"
      else do
        error "Invalid Index"

-- |List only completed tasks
-- Command Line: completed
process cfg ("completed":[]) = process' (todoTxtPath cfg) completed
  where
    completed = (\xss -> forM_ xss printTuple) . numberify
                                               . reverse
                                               . sort
                                               . onlyCompleted

-- |Append to a currently existing task
-- Command Line: append 1 Text to add to task 1
process cfg ("append":idx:rest) = process' (todoTxtPath cfg) appendIdx
  where
    appendIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ reverse $ sort $ onlyPending xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
        let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
        case matches of
          [m] -> do
            case validateLine (show m ++ " " ++ unwords rest) of
              Left e -> error $ show e
              Right updated -> do
                writeTodoTxt (todoTxtPath cfg) (updated:nonMatch)
                putStrLn $ "Updated Task: " ++ show updated
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
  putStrLn " help"

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
  if (length xs >= nIdx) && (nIdx > 0)
  then do
    let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
    let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
    let updated = map (\(Incomplete _ d pro con str) ->
                          Incomplete mPri d pro con str) matches
    writeTodoTxt path (updated ++ nonMatch)
    putStrLn "Updated Priority"
  else do
    error "Invalid Index"

