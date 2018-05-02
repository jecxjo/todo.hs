{-# LANGUAGE OverloadedStrings #-}

module Todo.HelpInfo (
  usage,
  commandList,
  commandHelp
  ) where

import           App.Resources
import           Todo.Commands.Helpers
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

fn :: Text -> Text
fn path = maybe "Missing Resource" id (getResource $ T.unpack path)

usage :: Text
usage = fn "usage.txt"


commandList :: Text
commandList = fn "commandList.txt"

commandHelp :: Text -> Text
commandHelp "add" = fn "commands/add.txt"
commandHelp "addx" = fn "commands/addx.txt"
commandHelp "append" = fn "commands/append_prepend.txt"
commandHelp "prepend" = fn "commands/append_prepend.txt"
commandHelp "archive" = fn "commands/archive.txt"
commandHelp "complete" = fn "commands/complete.txt"
commandHelp "completed" = fn "commands/completed.txt"
commandHelp "delete" = fn "commands/delete.txt"
commandHelp "list" = fn "commands/list.txt"
commandHelp "listpriority" = fn "commands/listpriority.txt"
commandHelp "priority" = fn "commands/priority.txt"
commandHelp "replace" = fn "commands/replace.txt"
commandHelp "search" = fn "commands/search.txt"
commandHelp "searchcompleted" = fn "commands/search.txt"
commandHelp "version" = fn "commands/version.txt"
commandHelp "due" = fn "commands/due.txt"
commandHelp "searcharchived" = fn "commands/search.txt"
commandHelp "projects" = fn "commands/projects.txt"
commandHelp "help" = fn "commands/help.txt"
commandHelp "usage" = fn "commands/help.txt"
commandHelp "swap" = fn "commands/swap.txt"
commandHelp "repeat" = fn "commands/repeat.txt"

commandHelp "todo.txt" = fn "todo.txt"

commandHelp cmd = "Unknown Command: " <> cmd <> "\n" <> commandList
