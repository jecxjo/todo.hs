{-# LANGUAGE FlexibleContexts #-}
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

module Todo.TasksSpec where

import Control.Monad.Reader (Reader, runReader, asks, runReaderT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Exception (evaluate)
import Control.Monad.Date
import Data.Maybe (fromJust)
import Data.Time.Calendar (fromGregorian, Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.LocalTime (TimeOfDay(..), makeTimeOfDayValid)
import Test.Hspec (Spec, describe, context, it, shouldBe, shouldThrow, anyErrorCall)
import Todo.App
import Todo.Tasks


data MockDate = MockDate
  { mockDay       :: Day
  , mockUTCTime   :: UTCTime
  }

instance {-# OVERLAPPING #-} MonadDate (Reader MockDate) where
  getDay = asks mockDay
  getUTCTime = asks mockUTCTime

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Task Data Types" $ do
    describe "KeyValue" $ do
      it "Shows String Key Value Pairs" $ do
        show (KVString "foo" "bar") `shouldBe` "foo:bar"

      it "Shows DueDate Key Value Pairs" $ do
        show (KVDueDate (fromGregorian 2017 2 1)) `shouldBe` "due:2017-02-01"

      it "Shows Threshold Key Value Pairs" $ do
        show (KVThreshold (fromGregorian 2017 2 1)) `shouldBe` "t:2017-02-01"

      it "Shows At Key Value Pairs, with single digit hr and min and padding" $ do
        show (KVAt (TimeOfDay 1 2 0)) `shouldBe` "at:0102"

      it "Shows At Key Value Pairs, with double digits" $ do
        show (KVAt (TimeOfDay 10 20 0)) `shouldBe` "at:1020"

    describe "Task" $ do
      context "Incomplete Task" $ do
        it "Shows Incomplete task with just text" $ do
          show (Incomplete Nothing Nothing [SOther "Example", SOther "task"]) `shouldBe` "Example task"

        it "Shows Incomplete task with date and text" $ do
          show (Incomplete Nothing (Just $ fromGregorian 2017 2 1) [SOther "Example", SOther "task"]) `shouldBe` "2017-02-01 Example task"

        it "Shows Incomplete task with priority and text" $ do
          show (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `shouldBe` "(A) Example task"

        it "Shows Incomplete task with Projects" $ do
          show (Incomplete Nothing Nothing [SOther "Example", SOther "task", SProject "ProjectName"]) `shouldBe` "Example task +ProjectName"

        it "Shows Incomplete task with Context" $ do
          show (Incomplete Nothing Nothing [SOther "Example", SOther "task", SContext "ContextName"]) `shouldBe` "Example task @ContextName"

        it "Shows Incomplete task with everything" $ do
          show (Incomplete (Just 'B') (Just $ fromGregorian 2017 2 1) [SOther "Example", SOther "task", SProject "ProjectName", SContext "ContextName"]) `shouldBe` "(B) 2017-02-01 Example task +ProjectName @ContextName"

      context "Completed Task" $ do
        it "Show Completed task with everything" $ do
          show (Completed (Just 'C') (Just $ fromGregorian 2017 2 2) (Just $ fromGregorian 2017 2 1) [SOther "Example", SOther "task", SProject "ProjectName", SContext "ContextName"]) `shouldBe` "x (C) 2017-02-02 2017-02-01 Example task +ProjectName @ContextName"

        it "Shows Completed task with no dates" $ do
          show (Completed (Just 'D') Nothing Nothing [SOther "Example"]) `shouldBe` "x (D) Example"

        it "Shows Completed task with no priority or dates" $ do
          show (Completed Nothing Nothing Nothing [SOther "Example"]) `shouldBe` "x Example"

      context "Compare Tasks" $ do
        it "Two Incomplete with no priority are equal" $ do
          (Incomplete Nothing Nothing [SOther "Example", SOther "task"]) == (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` True

        it "Two Incomplete with priority and none are not equal" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) == (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` False

        it "Two Incomplete with priority and none are not equal" $ do
          (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) == (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `shouldBe` False

        it "Two Incomplete with priorities that match are equal" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) == (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` True

        it "Two Incomplete with priorities that don't match are not equal" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) == (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` False

        it "Completed and Incomplete are not equal" $ do
          (Completed Nothing Nothing Nothing [SOther "Example", SOther "task"]) == (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` False

        it "Two Completed with no priority are equal" $ do
          (Completed Nothing  Nothing Nothing [SOther "Example", SOther "task"]) == (Completed Nothing Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` True

        it "Two Completed with priorities that match are equal" $ do
          (Completed (Just 'A') Nothing Nothing [SOther "Example", SOther "task"]) == (Completed (Just 'A') Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` True

        it "Two Incomplete priority is higher than no priority" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `compare` (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` LT

        it "Two Incomplete no priority is lower than priority" $ do
          (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `compare` (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `shouldBe` GT

        it "Two Incomplete A > B" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `compare` (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task"]) `shouldBe` LT

        it "Two Incomplete B < A" $ do
          (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task"]) `compare` (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` GT

      -- describe "convertToDate" $ do
      --   let day = fromGregorian 2017 3 13
      --   let date = UTCTime { utctDay = day, utctDayTime = secondsToDiffTime 43200 }
      --   let mockData = MockDate
      --         { mockDay = day
      --         , mockUTCTime = date
      --         }

      --   it "Matches 'today'" $ do
      --     res <- runExceptT (runReaderT $ convertToDate "today" :: ExceptT ErrorType (Reader MockDate) Day) mockData
      --     res `shouldBe` Right day

      --   it "Matches 'yesterday'" $ do
      --     let res = unTestFixture (convertToDate "yesterday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 12)

      --   it "Matches 'tomorrow'" $ do
      --     let res = unTestFixture (convertToDate "tomorrow") fixture
      --     res `shouldBe` (fromGregorian 2017 3 14)

      --   it "Matches 'Monday' of next week" $ do
      --     let res = unTestFixture (convertToDate "Monday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 20)

      --   it "Matches 'Tuesday' of this week" $ do
      --     let res = unTestFixture (convertToDate "Tuesday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 14)

      --   it "Matches 'Wednesday' of this week" $ do
      --     let res = unTestFixture (convertToDate "Wednesday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 15)

      --   it "Matches 'Thursday' of this week" $ do
      --     let res = unTestFixture (convertToDate "Thursday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 16)

      --   it "Matches 'Friday' of this week" $ do
      --     let res = unTestFixture (convertToDate "Friday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 17)

      --   it "Matches 'Saturday' of this week" $ do
      --     let res = unTestFixture (convertToDate "Saturday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 18)

      --   it "Matches 'Sunday' of this week" $ do
      --     let res = unTestFixture (convertToDate "Sunday") fixture
      --     res `shouldBe` (fromGregorian 2017 3 19)

      --   it "Throws an error when not a day of the week" $ do
      --     evaluate (unTestFixture (convertToDate "NotADay") fixture) `shouldThrow` anyErrorCall


      -- describe "convertStringTypes" $ do
      --   let day = fromGregorian 2017 3 12
      --   let date = UTCTime { utctDay = day, utctDayTime = secondsToDiffTime 43200 }
      --   let kvs = [ SKeyValue $ KVString "due" "today"
      --             , SKeyValue $ KVString "t" "today"
      --             , SKeyValue $ KVString "at" "noon"
      --             ]
      --   let fixture = def { _getDay = return day
      --                     , _getUTCTime = return date
      --                     }
      --   it "Converts array" $ do
      --     let res = unTestFixture (convertStringTypes kvs) fixture
      --     res `shouldBe` [ SKeyValue $ KVDueDate day
      --                    , SKeyValue $ KVThreshold day
      --                    , SKeyValue $ KVAt (fromJust $ makeTimeOfDayValid 12 0 0)
      --                    ]
