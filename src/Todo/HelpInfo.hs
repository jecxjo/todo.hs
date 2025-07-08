{-# LANGUAGE OverloadedStrings #-}

module Todo.HelpInfo (
  usage,
  commandList,
  helpTopics,
  license,
  changelog
  ) where

import           App.Resources
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

fn :: Text -> Text
fn path = fromMaybe "Missing Resource" (getResource $ T.unpack path)

usage :: Text
usage = fn "usage.txt"

commandList :: Text
commandList = fn "commandList.txt"

helpTopics :: Text -> Text
helpTopics "add" = fn "commands/add.txt"
helpTopics "addx" = fn "commands/addx.txt"
helpTopics "all" = fn "commands/all.txt"
helpTopics "append" = fn "commands/append_prepend.txt"
helpTopics "prepend" = fn "commands/append_prepend.txt"
helpTopics "archive" = fn "commands/archive.txt"
helpTopics "complete" = fn "commands/complete.txt"
helpTopics "completed" = fn "commands/completed.txt"
helpTopics "yesterday" = fn "commands/yesterday.txt"
helpTopics "delete" = fn "commands/delete.txt"
helpTopics "list" = fn "commands/list.txt"
helpTopics "listpriority" = fn "commands/listpriority.txt"
helpTopics "priority" = fn "commands/priority.txt"
helpTopics "replace" = fn "commands/replace.txt"
helpTopics "search" = fn "commands/search.txt"
helpTopics "seachall" = fn "commands/seach.txt"
helpTopics "searchcompleted" = fn "commands/search.txt"
helpTopics "version" = fn "commands/version.txt"
helpTopics "due" = fn "commands/due.txt"
helpTopics "searcharchived" = fn "commands/search.txt"
helpTopics "projects" = fn "commands/projects.txt"
helpTopics "help" = fn "commands/help.txt"
helpTopics "usage" = fn "commands/help.txt"
helpTopics "license" = fn "commands/help.txt"
helpTopics "changelog" = fn "commands/help.txt"
helpTopics "swap" = fn "commands/swap.txt"
helpTopics "repeat" = fn "commands/repeat.txt"
helpTopics "standup" = fn "commands/standup.txt"
helpTopics "today" = fn "commands/today.txt"
helpTopics "listaddons" = fn "commands/listaddons.txt"

helpTopics "todo.txt" = fn "topics/todo.txt"
helpTopics "duedate" = fn "topics/duedate.txt"
helpTopics "threshold" = fn "topics/threshold.txt"
helpTopics "at" = fn "topics/at.txt"
helpTopics "addons" = fn "topics/addons.txt"

helpTopics cmd = "Unknown Command: " <> cmd <> "\n" <> commandList

license :: Text
license = getLicense

changelog :: Text
changelog = getChangeLog
