module Main where

import Todo.App
import Todo.Commands (parseArgs)
import Todo.Commands.Helpers (todoFilePath)

main :: IO ()
main = do
  defaultPath <- todoFilePath
  runApp (initOptions defaultPath) parseArgs
