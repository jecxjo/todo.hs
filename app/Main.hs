module Main where

import Todo.App
import Todo.Commands (parseArgs)
import Todo.Commands.Helpers (todoFilePath)

main :: IO ()
main = do
  defaultPath <- todoFilePath
  runApp (Options defaultPath Nothing Nothing Nothing Nothing) parseArgs
