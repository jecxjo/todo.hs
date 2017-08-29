module CommandsSpec where

import Commands (process, ConfigOption(..))
import Util (removeIfExists)

import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.FilePath (joinPath)
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  , shouldSatisfy
                  , shouldThrow
                  , before_
                  , errorCall)
import Text.Regex.Posix ((=~))

-- |Helper function to read the contents of the todo.txt file
getTxtFile :: FilePath -> IO String
getTxtFile path = do
  exists <- doesFileExist path
  if exists
  then do
    lns <- readFile path
    return lns
  else do
    let lns = ""
    return lns

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Processing command line arguments" $ do
    let cfg = ConfigOption { todoTxtPath = joinPath [".", "todo-testing.txt"]
                           , archiveTxtPath = Just $ joinPath [".", "archive-testing.txt"]
                           , reportTxtPath = Just $ joinPath [".", "report-testing.txt"]
                           , timeStamp = Nothing }

    describe "Add action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "creates a new empty todo.txt" $ do
          process cfg []
          doesFileExist (todoTxtPath cfg) >>= shouldBe True

        it "adds new entry with no priority or date" $ do
          process cfg (words "add An example task")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "An example task")

        it "adds a new entry with priority, date, context and project" $ do
          process cfg (words "add (A) 2017-03-01 An example task for +ProjectName with @Context")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "(A) 2017-03-01 An example task for +ProjectName with @Context")

        it "adds a new entry with auto timestamping" $ do
          process cfg (words "-s add (A) An example with auto timestamp")
          getTxtFile (todoTxtPath cfg) >>= (`shouldSatisfy` (\x -> x =~ ("\\(A\\) [0-9]{4}-[0-9]{2}-[0-9]{2} An example with auto timestamp")))

    describe "Append action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "appends to single entry" $ do
          process cfg (words "add An example")
          process cfg (words "append 1 task")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "An example task")

        it "appends a middle entry" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          process cfg (words "append 2 in +Project")
          getTxtFile (todoTxtPath cfg) >>= (`shouldSatisfy` (\x -> x =~ ("Task 1\\\n" ++
                                                                          "Task 3\\\n" ++
                                                                          "Task 2 in \\+Project") :: Bool))

        it "does not append an invalid index" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "append 3 +FooBar") `shouldThrow` (errorCall "Invalid Index: append index \"text to append\"")

    describe "Prepend action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "prepends to single entry" $ do
          process cfg (words "add example task")
          process cfg (words "prepend 1 An")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "An example task")

        it "prepends a middle entry" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          process cfg (words "prepend 2 +Project has")
          getTxtFile (todoTxtPath cfg) >>= (`shouldSatisfy` (\x -> x =~ ("Task 1\\\n" ++
                                                                          "Task 3\\\n" ++
                                                                          "\\+Project has Task 2") :: Bool))

    describe "Replace action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "replaces text only" $ do
          process cfg (words "add example task")
          process cfg (words "replace 1 foo bar")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "foo bar")

        it "replaces text and keeps priority" $ do
          process cfg (words "add (A) example task")
          process cfg (words "replace 1 foo bar")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "(A) foo bar")

        it "replaces text and priority" $ do
          process cfg (words "add (A) example task")
          process cfg (words "replace 1 (B) foo bar")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "(B) foo bar")

        it "replaces text and keeps timestamp" $ do
          process cfg (words "add 2017-04-07 example task")
          process cfg (words "replace 1 foo bar")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "2017-04-07 foo bar")

        it "replaces text and timestamp" $ do
          process cfg (words "add 2017-04-07 example task")
          process cfg (words "replace 1 2017-04-08 foo bar")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "2017-04-08 foo bar")

    describe "Complete action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "completes a new entry with no priority or date" $ do
          process cfg (words "add An example task")
          process cfg (words "complete 1")
          getTxtFile (todoTxtPath cfg) >>= (`shouldSatisfy` (\x -> x =~ "^x [0-9]{4}-[0-9]{2}-[0-9]{2} An example task$" :: Bool))

        it "completes a new entry with priority and date" $ do
          process cfg (words "add (A) 2017-03-01 An example task for +ProjectName with @Context")
          process cfg (words "complete 1")
          getTxtFile (todoTxtPath cfg) >>= (`shouldSatisfy` (\x -> x =~ "^x [0-9]{4}-[0-9]{2}-[0-9]{2} \\(A\\) 2017-03-01 An example task for \\+ProjectName with \\@Context$" :: Bool))

        it "completes 2nd of three tasks" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          process cfg (words "complete 2")
          getTxtFile (todoTxtPath cfg) >>= (`shouldSatisfy` (\x -> x =~ ("Task 1\\\n" ++
                                                             "Task 3\\\n" ++
                                                             "x [0-9]{4}-[0-9]{2}-[0-9]{2} Task 2") :: Bool))

        it "does not complete an invalid index" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "complete 3") `shouldThrow` (errorCall "Invalid Index: complete index")

    describe "Delete action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "deletes a single entry" $ do
          process cfg (words "add Task 1")
          process cfg (words "delete 1")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "")

        it "deletes a middle entry" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          process cfg (words "delete 2")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "Task 1\nTask 3")

        it "does not delete an invalid index" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "delete 3") `shouldThrow` (errorCall "Invalid Index: delete index")

    describe "Priority action" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "adds a priority to unprioritized task" $ do
          process cfg (words "add Task 1")
          process cfg (words "priority 1 A")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "(A) Task 1")

        it "modifies a priority to a prioritized task" $ do
          process cfg (words "add (A) Task 1")
          process cfg (words "priority 1 B")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "(B) Task 1")

        it "removes a priority to a prioritized task" $ do
          process cfg (words "add (A) Task 1")
          process cfg (words "priority 1")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "Task 1")

        it "fails on invalid index" $ do
          process cfg (words "add (A) Task 1")
          process cfg (words "priority 2 B") `shouldThrow` (errorCall "Invalid Index: priority index [priority]")

        it "fails on invalid priority" $ do
          process cfg (words "add (A) Task 1")
          process cfg (words "priority 1 5") `shouldThrow` (errorCall "Invalid Priority: Valid values A-Z or left blank for no priority")

        it "allows lower case priority" $ do
          process cfg (words "add Task 1")
          process cfg (words "priority 1 a")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "(A) Task 1")

    describe "Order of entries" $ do
      before_ (removeIfExists $ todoTxtPath cfg) $ do
        it "adds in order" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "Task 1\nTask 2\nTask 3")


        it "Adds, delete and adds again" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          process cfg (words "delete 2")
          process cfg (words "add Task 4")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "Task 1\nTask 3\nTask 4")

        it "Adds and appends" $ do
          process cfg (words "add Task 1")
          process cfg (words "add Task 2")
          process cfg (words "add Task 3")
          process cfg (words "append 2 foo")
          getTxtFile (todoTxtPath cfg) >>= (`shouldBe` "Task 1\nTask 3\nTask 2 foo")

    describe "Archives completed tasks" $ do
      let iPath = todoTxtPath cfg
      let cPath = archiveTxtPath cfg
      before_ (removeIfExists iPath) $ do
        before_ (removeIfExists $ fromJust cPath) $ do
          it "removes completed" $ do
            process cfg (words "add Task 1")
            process cfg (words "add Task 2")
            process cfg (words "add Task 3")
            process cfg (words "add Task 4")
            process cfg (words "complete 2")
            process cfg (words "complete 3")
            process cfg (words "archive")
            getTxtFile iPath >>= (`shouldBe` "Task 1\nTask 3")

          it "archives completed" $ do
            process cfg (words "add Task 1")
            process cfg (words "add Task 2")
            process cfg (words "add Task 3")
            process cfg (words "add Task 4")
            process cfg (words "complete 2")
            process cfg (words "complete 3")
            process cfg (words "archive")
            getTxtFile (fromJust cPath) >>= (`shouldSatisfy` (\x -> x =~ ("^x [0-9]{4}-[0-9]{2}-[0-9]{2} Task 4\\\nx [0-9]{4}-[0-9]{2}-[0-9]{2} Task 2") :: Bool))

          it "archives appends completed" $ do
            process cfg (words "add Task 1")
            process cfg (words "add Task 2")
            process cfg (words "add Task 3")
            process cfg (words "add Task 4")
            process cfg (words "complete 2")
            process cfg (words "archive")
            process cfg (words "complete 3")
            process cfg (words "archive")
            getTxtFile (fromJust cPath) >>= (`shouldSatisfy` (\x -> x =~ ("^x [0-9]{4}-[0-9]{2}-[0-9]{2} Task 2\\\nx [0-9]{4}-[0-9]{2}-[0-9]{2} Task 4") :: Bool))

    describe "Report of incomplete and completed tasks" $ do
      let iPath = todoTxtPath cfg
      let rPath = reportTxtPath cfg
      let cPath = archiveTxtPath cfg
      before_ (removeIfExists iPath) $ do
        before_ (removeIfExists $ fromJust cPath) $ do
          before_ (removeIfExists $ fromJust rPath) $ do
            it "reports both numbers" $ do
              process cfg (words "add Task 1")
              process cfg (words "add Task 2")
              process cfg (words "add Task 3")
              process cfg (words "add Task 4")
              process cfg (words "add Task 5")
              process cfg (words "complete 2")
              process cfg (words "complete 3")
              process cfg (words "report")
              getTxtFile (fromJust rPath) >>= (`shouldSatisfy` (\x -> x =~ ("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2} 3 2") :: Bool))
