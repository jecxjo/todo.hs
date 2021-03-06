{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Todo.Commands where

import qualified Control.Exception as E
import           Control.Monad (forM_, liftM2, join)
import           Data.Bool (bool)
import           Data.Maybe (maybe)
import           Data.List (sort, nub, concatMap, sortBy)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Time.Calendar (addDays)
import           Data.Version (showVersion)
import           Paths_todo (version)
import           Todo.App
import           Todo.Commands.Helpers
import           Todo.HelpInfo
import           Todo.Parser
import           Todo.Tasks
import           Todo.RegEx (matchGen, swapGen, swapAllGen)
import           Todo.Util

-- | Main entry to command processing
parseArgs :: (AppConfig m, AppError m, MonadIO m, MonadArguments m, MonadFileSystem m, MonadDate m) => m ()
parseArgs = getArgs >>= process

-- | Given a list of command line arguments, this function performs the user
-- triggered action
process :: (AppConfig m, AppError m, MonadIO m, MonadArguments m, MonadFileSystem m, MonadDate m) => [Text] -> m ()

-- | No args -> Print todo list
process [] = getPendingTodo >>= filterThreshold >>= printTuple

-- | Flag for passing in the location of the todo.txt file
process ("-t":path:rest) =
  modify (\st -> st { todoTxtPath = T.unpack path }) >> process rest

-- | Flag for passing in the location of the archive.txt file
process ("-a":path:rest) =
  modify (\st -> st { archiveTxtPath = Just (T.unpack path) }) >> process rest

-- | Flag to auto start date all tasks
process ("-s":rest) =
  getDay >>=
  (\now -> modify (\st -> st { timeStamp = Just now })) >>
  process rest

-- | Flag for passing in the location of report.txt file
process ("-r":path:rest) =
  modify (\st -> st { reportTxtPath = Just (T.unpack path) }) >> process rest

-- | Flag for auto accepting/denying any queries
process ("-y":rest) =
  modify (\st -> st { autoAccept = Just True }) >> process rest

process ("-n":rest) =
  modify (\st -> st { autoAccept = Just False }) >> process rest

-- | Flag for forcing prompts on all modifying queries
process ("-p":rest) =
  modify (\st -> st { forcedPrompt = True }) >> process rest

-- | List all entries, ignoring thresholds
-- Command Line: all "string to match" +Project @Context
process ("all":filters) =
  (filter (containsText filters . snd) <$> getAllTodo) >>=
  printTuple

-- |List entries with project and context filter
-- Command Line: list "string to match" +Project @Context
process ("list":filters) =
  (filter (containsText filters . snd) <$> (getPendingTodo >>= filterThreshold)) >>=
  printTuple

-- |List priority entries with project and context filters
process ("listpriority":filters) =
    (filter (onlyPriority . snd) . filter (containsText filters . snd) <$> (getPendingTodo >>= filterThreshold)) >>=
    printTuple
  where
    onlyPriority (Incomplete (Just _) _ _) = True
    onlyPriority _ = False

-- |search for tasks matching regex
process ("search":filters) =
    (filter (matchFn . show . snd) <$> (getPendingTodo >>= filterThreshold)) >>=
    printTuple
  where
    matchFn = matchGen $ T.unpack $ T.unwords filters

-- | Search for tasks matching regex ignoring threshold
process ("searchall":filters) =
    (filter (matchFn . show . snd) <$> getAllTodo) >>=
    printTuple
  where
    matchFn = matchGen $ T.unpack $ T.unwords filters

-- |search completed tasks matching regex
-- Command Line: searchcompleted "foo"
process ("searchcompleted":filters) =
    (filter (matchFn . show . snd) <$> getCompletedTodo) >>=
    printTuple
  where
    matchFn = matchGen $ T.unpack $ T.unwords filters

-- |Add task to list
-- Command Line: add Example Task for +Project with @Context
process ("add":rest) = do
  line <- either (throwError . EParseError) return $ validateLine . T.unpack $ T.unwords rest
  todo <- convertTaskStrings line
  allTasks <- getAllTodo
  let newList = [(0, todo)] <> allTasks
  bool (shortCircuit "Nothing Added") (writeTodo newList) =<< queryConfirm [todo] "Add"
  liftIO . putStrLn $ "ADDED: " ++ show todo

-- |Add completed task to list
-- Command Line: addx Example Task that is done
process ("addx":rest) = do
  line <- either (throwError . EParseError) return $ validateLine . T.unpack $ T.unwords rest
  todo <- convertTaskStrings line
  allTasks <- getAllTodo
  now <- getDay
  let todo' = Completed now todo
  let newList = [(0, todo')] <> allTasks
  bool (shortCircuit "Nothing Completed") (writeTodo newList) =<< queryConfirm [todo] "Complete"
  liftIO . putStrLn $ "COMPLETED: " ++ show todo'

-- |Delete task
-- Command Line: delete 1 3
process("delete":idx) = do
  (match, nonMatch) <- join (splitIndexTasks <$> mapM readIndex idx <*> getPendingTodo)
  bool (throwError $ EMiscError "No tasks were deleted") (replacePending nonMatch *> liftIO (putStrLn "Task Deleted")) =<< queryAction (map snd match) "Delete"

-- |Mark Task Complete
-- Command Line: complete 1
process ["complete", idx] = do
  (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
  now <- getDay
  let completed = map (\(i, t) -> (i, Completed now t)) match
  bool (shortCircuit "Nothing to Complete") (replacePending (nonMatch <> completed)) =<< queryConfirm (map snd match) "Complete"
  liftIO $ putStrLn "Task Completed"

process ("complete":idx) = do
  (match, nonMatch) <- join (splitIndexTasks <$> mapM readIndex idx <*> getPendingTodo)
  now <- getDay
  let completed = map (\(i, t) -> (i, Completed now t)) match
  bool (throwError $ EMiscError "No tasks were completed") (replacePending (nonMatch <> completed) *> liftIO (putStrLn "Task Completed")) =<< queryAction (map snd match) "Complete"

-- |List only completed tasks
-- Command Line: completed
process ["completed"] = getCompletedTodo >>= printTuple

-- |Append to a currently existing task
-- Command Line: append 1 Text to add to task 1
process ("append":idx:rest) = do
    (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
    appended <- applyRest match
    let numbered = zipWith (\i t -> (i, t)) [1..] appended
    bool (shortCircuit "Nothing to modify") (replacePending (nonMatch <> numbered)) =<< queryConfirm appended "Modify"
    printPrefixedTuple "Task Modified" numbered
  where
    applyRest = mapM (doRest . snd)
    doRest t = either (throwError . EParseError) convertTaskStrings $ validateLine (show t <> " " <> T.unpack (T.unwords rest))

-- |Prepend to a currently existing task
-- Command Line: prepend 1 Text to add to task 1
process ("prepend":idx:rest) = do
    (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
    prepended <- applyRest match
    let numbered = zipWith (\i t -> (i, t)) [1..] prepended
    bool (shortCircuit "Nothing to modify") (replacePending (nonMatch <> numbered)) =<< queryConfirm prepended "Modify"
    printPrefixedTuple "Task Modified" numbered
  where
    applyRest = mapM (doRest . snd)
    doRest t = either (throwError . EParseError) convertTaskStrings $ validateLine (T.unpack (T.unwords rest) <> " " <> show t)

-- |Replace existing task with text
-- Command Line: replace 1 "New text for task 1"
process ("replace":idx:rest) = do
  (_, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
  line <- either (throwError . EParseError) return $ validateLine . T.unpack $ T.unwords rest
  todo <- convertTaskStrings line
  let newList = [(0, todo)] <> nonMatch
  bool (shortCircuit "Nothing to modify") (writeTodo newList) =<< queryConfirm [todo] "Replace"
  liftIO . putStrLn $ "Task Replaced: " ++ show todo

-- |Does a Regex find and swap with text
-- Command Line: swap 1 "old text" "new text"
process ["swap", idx, oldText, newText] = do
    (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
    swapped <- applySwap match
    let numbered = zipWith (\i t -> (i, t)) [1..] swapped
    bool (shortCircuit "Nothing to modify") (replacePending (nonMatch <> numbered)) =<< queryConfirm swapped "Modify"
    printPrefixedTuple "Task Modified" numbered
  where
    applySwap = mapM (doSwap . snd)
    swapFn = swapGen (T.unpack oldText) (T.unpack newText)
    doSwap t = either (throwError . EParseError) convertTaskStrings $ validateLine (swapFn $ show t)

-- |Modifies the priority of a previously existing task
-- Command Line: priorty 1 B
process ["priority", idx, priority] = do
    (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
    char <- bool (throwError $ EInvalidArg "Priority must be a letter")  (return $ T.head $ T.toUpper priority) $ T.length priority == 1
    changed <- applyPri char match
    let numbered = zipWith (\i t -> (i, t)) [1..] changed
    bool (shortCircuit "Nothing to modify") (replacePending (nonMatch <> numbered)) =<< queryConfirm changed "Modify"
    printPrefixedTuple "Task Modified" numbered
  where
    applyPri char = mapM (doPri char . snd)
    doPri char (Incomplete _ mDay stx) = either (throwError . EParseError) convertTaskStrings $ validateLine $ show (Incomplete (Just char) mDay stx)

process ["priority", idx] = do
  (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
  let changed = map (\(i, Incomplete _ mDay stx) -> (i, Incomplete Nothing mDay stx)) match
  bool (shortCircuit "Nothing to modify") (replacePending (nonMatch <> changed)) =<< queryConfirm (map snd changed) "Modify"
  printPrefixedTuple "Task Modified" changed

-- | List only due tasks
process ["due"] = do
    todo <- getPendingTodo
    now <- getDay
    let due = filterTupleDueDate now todo
    printTuple due

-- | Archive all completed tasks
process ["archive"] = do
  getCompletedTodo >>= writeArchive
  replaceCompleted []
  liftIO $ putStrLn "Tasks Archived"

-- | Search archived tasks
process ("searcharchived":filters) =
    (filter (matchFn . show . snd) <$> getArchivedTodo) >>=
    printTuple
  where
    matchFn = matchGen $ T.unpack $ T.unwords filters

-- | Generate report
process ["report"] = do
  process ["archive"] -- Do Archive first
  pendingCount <- length <$> getPendingTodo
  archivedCount <- length <$> getArchivedTodo
  writeReport pendingCount archivedCount
  liftIO $ putStrLn $ "Report Created: " ++ show pendingCount ++ " " ++ show archivedCount

-- | List tasks based on project
process ["projects"] = do
  pending <- getPendingTodo
  let splitTodo = concatMap splitTodoFn pending
  let projects = nub . sort $ map (\(p,_,_) -> p) splitTodo
  forM_ projects (printProjects splitTodo)
  where
    splitTodoFn (i, t) = map (\p -> (p, i, t)) (filter (\x -> head x == '+') (words $ replace '.' '-' $ show t))
    printProjects tasks project = do
      liftIO $ putStrLn $ "==== " ++ project ++ " ===="
      printTuple $ map (\(_,i,t) -> (i,t)) $ filter (\(p,_,_) -> p == project) tasks
      liftIO $ putStrLn ""

-- | Repeat a task by creating a new task and completing the original
process ["repeat", idx] = do
  (match, nonMatch) <- join (splitIndexTasks <$> ((\x -> return [x]) =<< readIndex idx) <*> getPendingTodo)
  now <- getDay
  let completed = map (\(i,t) -> (i, Completed now t)) match
  let new = map (\(i, Incomplete pri date str) -> (i, Incomplete pri (maybe Nothing (\_ -> Just now) date) str)) match
  let newList = new <> nonMatch <> completed
  bool (shortCircuit "Nothing changed") (replacePending newList) =<< queryConfirm (map snd new) "Repeat"
  liftIO $ putStrLn "Tasks Repeated"

-- | Print stuff completed yesterday and stuff due for today
process ["standup"] = do
    todo <- getPendingTodo
    completed <- liftM2 (++) getArchivedTodo getCompletedTodo
    now <- getDay
    let yesterday = addDays (-1) now

    let due = removeIndex $ filterTupleDueDate now todo
    let completedYesterday = removeIndex $ filterTupleCompleteDate yesterday completed

    liftIO $ putStrLn "Standup"
    liftIO $ putStrLn "========================"

    liftIO $ putStrLn $ "Completed " ++ show yesterday
    liftIO $ putStrLn "------------------------"
    printList completedYesterday
    liftIO $ putStrLn ""

    liftIO $ putStrLn $ "Due " ++ show now
    liftIO $ putStrLn "------------------------"
    printList due
    liftIO $ putStrLn ""
  where
    remove' (_, Completed _ task) = task
    remove' (_, task) = task
    removeIndex = map remove'

    printList :: (Show a, MonadIO m) => [a] -> m ()
    printList lst = forM_ lst (liftIO . print)

-- |Print tasks due today, ordered by "at:HHMM"
process ["today"] = do
    todo <- getAllTodo
    now <- getDay
    let forToday = filterTupleDueDate now todo
    let sorted = sortBy byAt forToday
    liftIO $ putStrLn $ "Today: " ++ show now
    liftIO $ putStrLn "-----------------"
    printTuple sorted
  where
    byAt (_, a) (_, b) =
      case (a, b) of
        (Incomplete _ _ aKV, Completed _ (Incomplete _ _ bKV)) -> compare (extractAt aKV) (extractAt bKV)
        (Completed _ (Incomplete _ _ aKV), Incomplete _ _ bKV) -> compare (extractAt aKV) (extractAt bKV)
        (Incomplete _ _ aKV, Incomplete _ _ bKV) -> compare (extractAt aKV) (extractAt bKV)
        (Completed _ (Incomplete _ _ aKV), Completed _ (Incomplete _ _ bKV)) -> compare (extractAt aKV) (extractAt bKV)
        _ -> EQ -- Don't care at that point its all screwed up

-- |Help output
-- Command Line: help
process ["usage"] = liftIO $ T.putStrLn usage
process ["help",rest] = liftIO $ T.putStrLn $ helpTopics rest
process ["help"] = liftIO $ T.putStrLn commandList

-- | Show Version
-- Command Line: version
process ["version"] = liftIO . putStrLn $ "Version: " ++ showVersion version

-- | Show license
-- Command Line: license
process ["license"] = liftIO $ T.putStrLn license

-- | Show changelog
-- Command Line: changelog
process ["changelog"] = liftIO $ T.putStrLn changelog

-- | Aliases
process ("remove":rest) = process ("delete":rest)
process ("del":rest) = process ("delete":rest)
process ("rm":rest) = process ("delete":rest)
process ("done":rest) = process ("complete":rest)
process ("do":rest) = process ("complete":rest)
process ("ls":rest) = process ("list":rest)
process ("s":rest) = process ("search":rest)
process ("pri":rest) = process ("priority":rest)
process ("sc":rest) = process ("searchcompleted":rest)
process ("sa":rest) = process ("searcharchived":rest)
process ("lsp":rest) = process ("listpriority":rest)
process ("-h":rest) = process ("help":rest)
process ("--help":rest) = process ("help":rest)
process ("app":rest) = process("append":rest)
process ("pre":rest) = process("prepend":rest)
process ("rep":rest) = process("replace":rest)

-- | Test Call
process ("test":d:_) = do
  today <- convertToDate $ T.unpack d
  liftIO . putStrLn $ show today

-- | Passing just a number prints index
process [idx] = do
  idx' <- maybe (throwError $ EInvalidArg idx) return (maybeRead (T.unpack idx) :: Maybe Int)
  pending <- getPendingTodo
  notEmpty (throwError $ EInvalidIndex idx') printTuple $ filter ((== idx') . fst) pending

-- | Fail over
process _ = throwError . EMiscError $ T.pack "Invalid argument"

