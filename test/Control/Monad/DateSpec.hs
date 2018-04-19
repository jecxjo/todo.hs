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
module Control.Monad.DateSpec where

import Control.Monad.Date
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)

mkFixture "Fixture" [ts| MonadDate |]

spec :: Spec
spec =
  describe "MonadDate" $ do
    let fixture = def { _getDay = return $ fromGregorian 2018 4 8
                      , _getUTCTime = return $ UTCTime { utctDay = fromGregorian 2018 4 8, utctDayTime = secondsToDiffTime 43200 }
                      }

    describe "getDay" $ do
      it "Shows YYYY-MM-DD" $ do
        let res = unTestFixture (getDay) fixture
        show res `shouldBe` "2018-04-08"

    describe "getUTCTime" $ do
      it "Shows YYYY-MM-DD HH:MM:SS ZZZ" $ do
        let res = unTestFixture (getUTCTime) fixture
        show res `shouldBe` "2018-04-08 12:00:00 UTC"
