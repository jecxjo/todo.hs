{-# LANGUAGE OverloadedStrings #-}
module Commands ( processArgs ) where

import Control.Monad (forM_)
import Data.List (sort, isPrefixOf)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

import FileHandler (readTodoTxt, writeTodoTxt)
import Parser (validateLine)
import Tasks (Task(..), Date(..), Project, Context, onlyPending, onlyCompleted,
              filterProjects, filterContext)
import Util (subsetOf)
import Version (version)

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

-- |Reads the file, print error or calls a function to handle the list of todo
-- items.
processArgs' :: ([Task] -> IO ()) -> IO ()
processArgs' f = do
  path <- todoFilePath
  todo <- readTodoTxt path
  case todo of
    Left e -> error (show e)
    Right xs -> do
      f xs

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

-- |Process command line arguments.
-- This function has multiple instances for performing the different actions.
processArgs :: [String] -> IO ()

-- |Default Command, list all entries
-- Command Line:
processArgs [] = processArgs' listAll
  where listAll = (\xss -> forM_ xss printTuple) . numberify
                                                 . reverse
                                                 . sort
                                                 . onlyPending

-- |List entries with project and context filter
-- Command Line: list +Project @Context
processArgs ("list":filters) = processArgs' listSome
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
processArgs ("add":rest) = processArgs' addToList
  where
    addToList oldLines = case validateLine (unwords rest) of
                            Left e -> error $ show e
                            Right newLine -> do
                              path <- todoFilePath
                              writeTodoTxt path (newLine:oldLines)
                              putStrLn $ "New Task: " ++ show newLine

-- |Delete task
-- Command Line: delete 1
processArgs ("delete":idx:[]) = processArgs' deleteIdx
  where
    deleteIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ reverse $ sort $ onlyPending xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        path <- todoFilePath
        writeTodoTxt path  $ denumbrify $ filter (\(n, _) -> n /= nIdx) xss
        putStrLn "Task Deleted"
      else do
        error "Invalid Index"

-- |Mark Task Complete
-- Command Line: complete 1
processArgs ("complete":idx:[]) = processArgs' completeIdx
  where
    completeIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ reverse $ sort $ onlyPending xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        path <- todoFilePath
        c <- getCurrentTime
        let (y, m, d) = toGregorian $ utctDay c
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
        let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
        let completed = map (\t -> Completed (Date y m d) t) matches
        writeTodoTxt path (nonMatch ++ completed)
        putStrLn "Task Completed"
      else do
        error "Invalid Index"

-- |List only completed tasks
-- Command Line: completed
processArgs ("completed":[]) = processArgs' completed
  where
    completed = (\xss -> forM_ xss printTuple) . numberify
                                               . reverse
                                               . sort
                                               . onlyCompleted

-- |Append to a currently existing task
-- Command Line: append 1 Text to add to task 1
processArgs ("append":idx:rest) = processArgs' appendIdx
  where
    appendIdx xs = do
      let nIdx = read idx :: Int
      let xss = numberify $ reverse $ sort $ onlyPending xs
      if (length xss >= nIdx) && (nIdx > 0)
      then do
        path <- todoFilePath
        let nonMatch = denumbrify $ filter (\(n,_) -> n /= nIdx) xss
        let matches = denumbrify $ filter (\(n,_) -> n == nIdx) xss
        case matches of
          [m] -> do
            case validateLine (show m ++ " " ++ unwords rest) of
              Left e -> error $ show e
              Right updated -> do
                writeTodoTxt path (updated:nonMatch)
                putStrLn $ "Updated Task: " ++ show updated
          _ -> do error "Error in append"
      else do
        error "Invalid Index"

-- |Help output
-- Command Line: help
processArgs ("help":_) = do
  putStrLn "Usage: todo action [task_number] [task_description]"
  putStrLn ""
  putStrLn "Actions:"
  putStrLn " add \"Task I need to do +project @context\""
  putStrLn " list|ls +project @context"
  putStrLn " del|delete|remove TASKNUM"
  putStrLn " complete|done TASKNUM"
  putStrLn " completed"
  putStrLn " version"
  putStrLn " append TASKNUM \"addition to task\""
  putStrLn " help"

-- |Show Version
-- Command Lin: version
processArgs ("version":_) = do
  putStrLn $ "Version: " ++ version

-- Aliases
processArgs ("remove":idx:[]) = processArgs("delete":idx:[])
processArgs ("del":idx:[]) = processArgs("delete":idx:[])
processArgs ("done":idx:[]) = processArgs("complete":idx:[])
processArgs ("ls":rest) = processArgs("list":rest)
processArgs _ = processArgs ("help":[])
