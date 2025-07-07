{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ArgumentsSpec where

import           Control.Monad.Arguments
import           Control.Monad.Reader (Reader, runReader, asks)
import           Data.Text (Text)
import           Test.Hspec (Spec, describe, it, shouldBe)

-- Data type to store mock arguments
data MockArguments = MockArguments
  { mockArgs       :: [Text]
  , mockArg0       :: Text
  , mockExecutable :: Text
  }

-- MonadArguments instance using Reader monad
instance {-# OVERLAPPING #-} MonadArguments (Reader MockArguments) where
  getArgs = asks mockArgs
  getArg0 = asks mockArg0
  getExecutable = asks mockExecutable

spec :: Spec
spec =
  describe "MonadArguments" $ do
    describe "getArgs" $ do
      it "returns a list of arguments" $ do
        let mockData = MockArguments ["Arg1", "Arg2", "Arg3"] "mockArg0" "/mock/path/to/executable"
        let result = runReader getArgs mockData
        result `shouldBe` ["Arg1", "Arg2", "Arg3"]

    describe "getArg0" $ do
      it "returns the first argument" $ do
        let mockData = MockArguments ["Arg1", "Arg2", "Arg3"] "mockArg0" "/mock/path/to/executable"
        let result = runReader getArg0 mockData
        result `shouldBe` "mockArg0"

    describe "getExecutable" $ do
      it "returns the executable path" $ do
        let mockData = MockArguments ["Arg1", "Arg2", "Arg3"] "mockArg0" "/mock/path/to/executable"
        let result = runReader getExecutable mockData
        result `shouldBe` "/mock/path/to/executable"
