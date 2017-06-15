module FileHandler
    (
      writeTodoTxt
    , appendTodoTxt
    , readTodoTxt
    , writeReportTxt
    ) where

import Data.List (intercalate)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Data.Time.LocalTime (localTimeOfDay, utcToLocalTime, getCurrentTimeZone, TimeOfDay(..))
import System.Directory (doesFileExist)

import Parser (parseLines, ParseError)
import Tasks (Task)

-- |Write to todo.txt file
writeTodoTxt :: FilePath -> [Task] -> IO ()
writeTodoTxt path tx = do
    writeFile path str
  where str = intercalate "\n" $ map show tx

-- |Append to todo.txt file
appendTodoTxt :: FilePath -> [Task] -> IO ()
appendTodoTxt path tx = do
  exists <- doesFileExist path
  if exists
  then do
    oldComplete <- readTodoTxt path
    case oldComplete of
      Right oldTx -> (writeFile path . intercalate "\n") . map show $ (oldTx ++ tx)
      Left err -> putStrLn $ "Error: " ++ show err
  else
    writeFile path . intercalate "\n" $ map show tx

-- |Read from todo.txt
-- If no file exists then one is created.
readTodoTxt :: FilePath -> IO (Either ParseError [Task])
readTodoTxt path = do
  exists <- doesFileExist path
  if exists
  then do
    lns <- readFile path
    return $ parseLines path lns
  else do
      writeTodoTxt path []
      lns <- readFile path
      return $ parseLines path lns

-- |Writes to report.txt
writeReportTxt :: FilePath -> Int -> Int -> IO ()
writeReportTxt path incomp comp = do
    c <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (y,m,d) = toGregorian $ utctDay c
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone c
    let str = showFour y ++ "-" ++ showTwo m ++ "-" ++ showTwo d ++ "T"
              ++ showTwo hour ++ ":" ++ showTwo minute ++ ":" ++ (showTwo $ truncate second)
              ++ " " ++ show incomp ++ " " ++ show comp
    exists <- doesFileExist path
    if exists
    then do
      appendFile path $ "\n" ++ str
    else
      writeFile path str
  where
    showTwo :: Int -> String
    showTwo i = if i < 10
                then
                  "0" ++ show i
                else
                  show i
    showFour :: Integer -> String
    showFour i
      | i < 10 = "000" ++ show i
      | i < 100 = "00" ++ show i
      | i < 1000 = "0" ++ show i
      | otherwise = show i
