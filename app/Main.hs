module Main where

import System.Environment (getArgs)

import Commands (processArgs)

main :: IO ()
main = do
  args <- getArgs
  processArgs args
