{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.FileSystemSpec where

import           Control.Monad.FileSystem

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.List (find, sortOn)
import           Data.Maybe (maybe)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (readFile, writeFile, appendFile)
import           Test.Hspec (Spec, describe, it, shouldBe)

-- | Test type for verifying MonadFileSystem
data FileT = FileT { fileName :: Text, fileData :: Text }
  deriving (Show, Eq)

mkFixture "Fixture" [ts| MonadFileSystem |]

spec :: Spec
spec =
  describe "MonadFileSystem" $ do
    let filesystem = [FileT { fileName = "todo.txt", fileData = "Unit Test @todo app" },
                      FileT { fileName = "done.txt", fileData = T.empty }
                     ]
    let fixture = def { _readFile = \path -> do
                                        files <- get
                                        maybe (return "") (return . fileData) (find (\f -> path == fileName f) files)
                      , _writeFile = \path text -> do
                                        files <- get
                                        let otherFiles = filter (\f -> path /= fileName f) files
                                        let newFiles = sortOn fileName $ [FileT { fileName = path, fileData = text }] <> otherFiles
                                        put newFiles
                      , _appendFile = \path text -> do
                                        files <- get
                                        let oldText = maybe T.empty fileData (find (\f -> path == fileName f) files)
                                        let otherFiles = filter (\f -> path /= fileName f) files
                                        let newFiles = sortOn fileName $ [FileT { fileName = path, fileData = oldText <> text }] <> otherFiles
                                        put newFiles
                      }

    describe "readFile" $ do
      it "returns an empty string when no file exists" $ do
        let (file, _, _) = runTestFixture (readFile "test.txt") fixture filesystem
        file `shouldBe` T.empty

      it "returns contents when a file exists" $ do
        let (file, _, _) = runTestFixture (readFile "todo.txt") fixture filesystem
        file `shouldBe` (fileData $ (!!) filesystem 0)

    describe "writeFile" $ do
      it "creates a file when one doesn't exist" $ do
        let newFile = FileT { fileName = "test.txt",  fileData = "This is a test" }
        let (_, filesystem', _) = runTestFixture (writeFile (fileName newFile) (fileData newFile)) fixture filesystem
        let expected = sortOn fileName $ [newFile] <> filesystem
        filesystem' `shouldBe` expected
        length filesystem' `shouldBe` 3

      it "replaces a file when one exists" $ do
        let newFile = FileT { fileName = "todo.txt",  fileData = "This is a test" }
        let (_, filesystem', _) = runTestFixture (writeFile (fileName newFile) (fileData newFile)) fixture filesystem
        let expected = sortOn fileName $ [newFile] <> (filter (\f -> "todo.txt" /= fileName f) filesystem)
        filesystem' `shouldBe` expected
        length filesystem' `shouldBe` 2

    describe "appendFile" $ do
      it "creates a file when one doesn't exist" $ do
        let newFile = FileT { fileName = "test.txt",  fileData = "This is a test" }
        let (_, filesystem', _) = runTestFixture (appendFile (fileName newFile) (fileData newFile)) fixture filesystem
        let expected = sortOn fileName $ [newFile] <> filesystem
        filesystem' `shouldBe` expected
        length filesystem' `shouldBe` 3

      it "replaces a file when one exists" $ do
        let newFile = FileT { fileName = "todo.txt",  fileData = "\nThis is a test" }
        let (_, filesystem', _) = runTestFixture (appendFile (fileName newFile) (fileData newFile)) fixture filesystem
        let expectedFile = FileT { fileName = "todo.txt", fileData = "Unit Test @todo app\nThis is a test" }
        let expected = sortOn fileName $ [expectedFile] <> (filter (\f -> "todo.txt" /= fileName f) filesystem)
        filesystem' `shouldBe` expected
        length filesystem' `shouldBe` 2
