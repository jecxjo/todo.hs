{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.DateSpec where

import Control.Monad.Date
import Control.Monad.Reader (Reader, runReader, asks)
import Data.Time.Calendar (fromGregorian, Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)

data MockDate = MockDate
  { mockDay       :: Day
  , mockUTCTime   :: UTCTime
  }

instance {-# OVERLAPPING #-} MonadDate (Reader MockDate) where
  getDay = asks mockDay
  getUTCTime = asks mockUTCTime

spec :: Spec
spec =
  describe "MonadDate" $ do
    let mockData = MockDate
          { mockDay = fromGregorian 2018 4 8
          , mockUTCTime = UTCTime (fromGregorian 2018 4 8) (secondsToDiffTime 43200) -- 12:00:00 UTC
          }
    describe "getDay" $ do
      it "Shows YYYY-MM-DD" $ do
        let result = runReader getDay mockData
        show result `shouldBe` "2018-04-08"

    describe "getUTCTime" $ do
      it "Shows YYYY-MM-DD HH:MM:SS ZZZ" $ do
        let result = runReader getUTCTime mockData
        show result `shouldBe` "2018-04-08 12:00:00 UTC"
