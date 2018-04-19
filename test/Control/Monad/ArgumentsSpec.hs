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
module Control.Monad.ArgumentsSpec where

import           Control.Monad.Arguments
import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec (Spec, describe, it, shouldBe)

mkFixture "Fixture" [ts| MonadArguments |]

spec :: Spec
spec =
  describe "MonadArguments" $ do
    describe "getArgs" $ do
      it "can return an empty list of arguments" $ do
        let fixture = def { _getArgs = return [] }
        let result = unTestFixture (getArgs) fixture
        result `shouldBe` []

      it "can return a list of arguments" $ do
        let fixture = def { _getArgs = return ["Arg1", "Arg2", "Arg3"] }
        let result = unTestFixture (getArgs) fixture
        result `shouldBe` ["Arg1", "Arg2", "Arg3"]

