{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.FileSystemSpec where

import           Control.Monad.FileSystem

import qualified Control.Exception as E
import           Control.Monad.State (State, get, put, runState)
import           Data.List (find, sortOn)
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Hspec (Spec, describe, it, shouldBe)
import           System.IO.Error (mkIOError, userErrorType)

-- File representation
data FileT = FileT
  { fileName :: Text
  , fileData :: Text
  } deriving (Eq, Show)

mockIOException :: E.IOException
mockIOException = mkIOError userErrorType "Mock IO Exception" Nothing Nothing

-- MonadFileSystem instance using State monad
instance {-# OVERLAPPING #-} MonadFileSystem (State [FileT]) where
  readFileSafe path = do
    files <- get
    case find (\f -> fileName f == path) files of
      Just file -> return $ Right (fileData file)
      Nothing -> return $ Left $ mockIOException

  writeFileSafe path text = do
    files <- get
    let otherFiles = filter (\f -> fileName f /= path) files
    let newFile = FileT path text
    let newFiles = (newFile : otherFiles)
    put newFiles
    return $ Right ()

  appendFileSafe path text = do
    files <- get
    let oldText = maybe T.empty fileData (find (\f -> fileName f == path) files)
    let otherFiles = filter (\f -> fileName f /= path) files
    let newFiles = sortOn fileName $ FileT path (oldText <> text) : otherFiles
    put newFiles
    return $ Right ()

  listFilesSafe _ = do
    files <- get
    return $ Right $ map fileName files

spec :: Spec
spec =
  describe "MonadFileSystem" $ do

    describe "readFileSafe" $ do
      it "returns contents when a file exists" $ do
        let initialFilesystem = [FileT "todo.txt" "Unit Test @todo app", FileT "done.txt" T.empty]
        let (result, _) = runState (readFileSafe "todo.txt" :: State [FileT] (Either E.IOException Text)) initialFilesystem
        result `shouldBe` Right "Unit Test @todo app"

      it "returns an error when a file does not exist" $ do
        let initialFilesystem = [FileT "todo.txt" "Unit Test @todo app", FileT "done.txt" T.empty]
        let (result, _) = runState (readFileSafe "test.txt" :: State [FileT] (Either E.IOException Text)) initialFilesystem
        result `shouldBe` Left mockIOException

    describe "writeFileSafe" $ do
      it "creates a file when one doesn't exist" $ do
        let initialFilesystem = []
        let (result, finalState) = runState (writeFileSafe "foo.txt" "This is a test" :: State [FileT] (Either E.IOException ())) initialFilesystem
        result `shouldBe` Right ()
        length finalState `shouldBe` 1
        finalState `shouldBe` [FileT "foo.txt" "This is a test"]

      it "replaces a file when one exists" $ do
        let initialFilesystem = [FileT "todo.txt" "Unit Test @todo app"]
        let (result, finalState) = runState (writeFileSafe "todo.txt" "This is a test" :: State [FileT] (Either E.IOException ())) initialFilesystem
        result `shouldBe` Right ()
        length finalState `shouldBe` 1
        finalState `shouldBe` [FileT "todo.txt" "This is a test"]

    describe "appendFileSafe" $ do
      it "creates a file when one doesn't exist" $ do
        let initialFilesystem = []
        let (result, finalState) = runState (appendFileSafe "foo.txt" "This is a test" :: State [FileT] (Either E.IOException ())) initialFilesystem
        result `shouldBe` Right ()
        length finalState `shouldBe` 1
        finalState `shouldBe` [FileT "foo.txt" "This is a test"]

      it "appends a file when it does exist" $ do
        let initialFilesystem = [FileT "todo.txt" "Unit Test @todo app"]
        let (result, finalState) = runState (appendFileSafe "todo.txt" "This is a test" :: State [FileT] (Either E.IOException ())) initialFilesystem
        result `shouldBe` Right ()
        length finalState `shouldBe` 1
        finalState `shouldBe` [FileT "todo.txt" "Unit Test @todo appThis is a test"]

    describe "listFilesSafe" $ do
      it "shows no files when there are no files" $ do
        let initialFilesystem = []
        let (result, _) = runState (listFilesSafe "" :: State [FileT] (Either E.IOException [Text])) initialFilesystem
        result `shouldBe` Right []

      it "shows files when there are files" $ do
        let initialFilesystem = [FileT "todo.txt" "Unit Test @todo app", FileT "done.txt" T.empty]
        let (result, _) = runState (listFilesSafe "" :: State [FileT] (Either E.IOException [Text])) initialFilesystem
        result `shouldBe` Right ["todo.txt", "done.txt"]
