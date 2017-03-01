module CommandsSpec where

import Commands (process)
import Util (removeIfExists)

import Control.Exception (ErrorCall(..))
import System.Directory (doesFileExist)
import System.FilePath (joinPath)
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  , shouldSatisfy
                  , shouldThrow
                  , before_)
import Text.Regex.Posix ((=~))

-- |Helper function to read the contents of the todo.txt file
getTodoFile :: FilePath -> IO String
getTodoFile path = do
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
    let path = joinPath [".", "todo-testing.txt"]

    describe "Add action" $ do
      before_ (removeIfExists path) $ do
        it "creates a new empty todo.txt" $ do
          process path []
          doesFileExist path >>= shouldBe True

        it "adds new entry with no priority or date" $ do
          process path (words "add An example task")
          getTodoFile path >>= (`shouldBe` "An example task")

        it "adds a new entry with priority, date, context and project" $ do
          process path (words "add (A) 2017-03-01 An example task for +ProjectName with @Context")
          getTodoFile path >>= (`shouldBe` "(A) 2017-03-01 An example task for +ProjectName with @Context")

    describe "Append action" $ do
      before_ (removeIfExists path) $ do
        it "appends to single entry" $ do
          process path (words "add An example")
          process path (words "append 1 task")
          getTodoFile path >>= (`shouldBe` "An example task")

        it "appends a middle entry" $ do
          process path (words "add Task 1")
          process path (words "add Task 2")
          process path (words "add Task 3")
          process path (words "append 2 in +Project")
          getTodoFile path >>= (`shouldSatisfy` (\x -> x =~ ("Task 2 in \\+Project\\\n" ++
                                                             "Task 1\\\n" ++
                                                             "Task 3") :: Bool))

        it "does not append an invalid index" $ do
          process path (words "add Task 1")
          process path (words "add Task 2")
          process path (words "append 3 +FooBar") `shouldThrow` (== ErrorCall "Invalid Index")

    describe "Complete action" $ do
      before_ (removeIfExists path) $ do
        it "completes a new entry with no priority or date" $ do
          process path (words "add An example task")
          process path (words "complete 1")
          getTodoFile path >>= (`shouldSatisfy` (\x -> x =~ "^x [0-9]{4}-[0-9]{2}-[0-9]{2} An example task$" :: Bool))

        it "completes a new entry with priority and date" $ do
          process path (words "add (A) 2017-03-01 An example task for +ProjectName with @Context")
          process path (words "complete 1")
          getTodoFile path >>= (`shouldSatisfy` (\x -> x =~ "^x [0-9]{4}-[0-9]{2}-[0-9]{2} \\(A\\) 2017-03-01 An example task for \\+ProjectName with \\@Context$" :: Bool))

        it "completes 2nd of three tasks" $ do
          process path (words "add Task 1")
          process path (words "add Task 2")
          process path (words "add Task 3")
          process path (words "complete 2")
          getTodoFile path >>= (`shouldSatisfy` (\x -> x =~ ("Task 1\\\n" ++
                                                             "Task 3\\\n" ++
                                                             "x [0-9]{4}-[0-9]{2}-[0-9]{2} Task 2") :: Bool))

        it "does not complete an invalid index" $ do
          process path (words "add Task 1")
          process path (words "add Task 2")
          process path (words "complete 3") `shouldThrow` (== ErrorCall "Invalid Index")

    describe "Delete action" $ do
      before_ (removeIfExists path) $ do
        it "deletes a single entry" $ do
          process path (words "add Task 1")
          process path (words "delete 1")
          getTodoFile path >>= (`shouldBe` "")

        it "deletes a middle entry" $ do
          process path (words "add Task 1")
          process path (words "add Task 2")
          process path (words "add Task 3")
          process path (words "delete 2")
          getTodoFile path >>= (`shouldBe` "Task 1\nTask 3")

        it "does not delete an invalid index" $ do
          process path (words "add Task 1")
          process path (words "add Task 2")
          process path (words "delete 3") `shouldThrow` (== ErrorCall "Invalid Index")
