module FileHandler
    (
      writeTodoTxt
    , readTodoTxt
    ) where

import Data.List (intercalate)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName, takeExtension)

import Parser (parseLines, ParseError)
import Tasks (Task)

-- |Write to todo.txt file
writeTodoTxt :: FilePath -> [Task] -> IO ()
writeTodoTxt path tx = do
    writeFile path str
  where str = intercalate "\n" $ map show tx

-- |Read from todo.txt
-- If no file exists then one is created.
readTodoTxt :: FilePath -> IO (Either ParseError [Task])
readTodoTxt path = do
  exists <- doesFileExist path
  if exists
  then do
    lines <- readFile path
    return $ parseLines path lines
  else do
      writeTodoTxt path []
      lines <- readFile path
      return $ parseLines path lines


-- |Write report file
writeReport :: FilePath -> Integer -> Integer -> IO ()
writeReport path incomplete complete = do
    appendFile reportFile str
  where
   str = "DATETIME " ++ show incomplete ++ " " ++ show complete
   reportFile = replaceFileName path $ "report" ++ takeExtension path
