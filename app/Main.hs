module Main where

import Todo.App
import Todo.Commands (parseArgs)
import Todo.Commands.Helpers (todoFilePath)
import System.Console.Pretty (supportsPretty)

main :: IO ()
main = do
  defaultPath <- todoFilePath
  canDoPretty <- supportsPretty
  runApp (initOptions defaultPath canDoPretty) parseArgs
