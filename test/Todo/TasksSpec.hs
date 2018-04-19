{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.TasksSpec where

import Control.Exception (evaluate)
import Control.Monad.Date
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Test.Hspec (Spec, describe, context, it, shouldBe, shouldThrow, anyErrorCall)
import Todo.App
import Todo.Tasks

mkFixture "FixtureInst" [ts| MonadError ErrorType, MonadDate |]

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Task Data Types" $ do
    describe "KeyValue" $ do
      it "Shows String Key Value Pairs" $ do
        show (KVString "foo" "bar") `shouldBe` "foo:bar"

      it "Shows DueDate Key Value Pairs" $ do
        show (KVDueDate (fromGregorian 2017 2 1)) `shouldBe` "due:2017-02-01"

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
          show (Completed (fromGregorian 2017 2 2) (Incomplete (Just 'C') (Just $ fromGregorian 2017 2 1) [SOther "Example", SOther "task", SProject "ProjectName", SContext "ContextName"])) `shouldBe` "x 2017-02-02 (C) 2017-02-01 Example task +ProjectName @ContextName"

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
          (Completed (fromGregorian 2017 2 2) (Incomplete Nothing Nothing [SOther "Example", SOther "task"])) == (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` False

        it "Two Completed with no priority are equal" $ do
          (Completed (fromGregorian 2017 2 2) (Incomplete Nothing Nothing [SOther "Example", SOther "task"])) == (Completed (fromGregorian 2017 2 2) (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"])) `shouldBe` True

        it "Two Completed with priorities that match are equal" $ do
          (Completed (fromGregorian 2017 2 2) (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"])) == (Completed (fromGregorian 2017 2 2) (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"])) `shouldBe` True

        it "Two Incomplete priority is higher than no priority" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `compare` (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` LT

        it "Two Incomplete no priority is lower than priority" $ do
          (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `compare` (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `shouldBe` GT

        it "Two Incomplete A > B" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `compare` (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task"]) `shouldBe` LT

        it "Two Incomplete B < A" $ do
          (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task"]) `compare` (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` GT

      describe "convertToDate" $ do
        let day = fromGregorian 2017 3 13
        let date = UTCTime { utctDay = day, utctDayTime = secondsToDiffTime 43200 }
        let fixture = def { _getDay = return day
                          , _getUTCTime = return date
                          }

        it "Matches 'today'" $ do
          let res = unTestFixture (convertToDate "today") fixture
          res `shouldBe` (fromGregorian 2017 3 13)

        it "Matches 'yesterday'" $ do
          let res = unTestFixture (convertToDate "yesterday") fixture
          res `shouldBe` (fromGregorian 2017 3 12)

        it "Matches 'tomorrow'" $ do
          let res = unTestFixture (convertToDate "tomorrow") fixture
          res `shouldBe` (fromGregorian 2017 3 14)

        it "Matches 'Monday' of next week" $ do
          let res = unTestFixture (convertToDate "Monday") fixture
          res `shouldBe` (fromGregorian 2017 3 20)

        it "Matches 'Tuesday' of this week" $ do
          let res = unTestFixture (convertToDate "Tuesday") fixture
          res `shouldBe` (fromGregorian 2017 3 14)

        it "Matches 'Wednesday' of this week" $ do
          let res = unTestFixture (convertToDate "Wednesday") fixture
          res `shouldBe` (fromGregorian 2017 3 15)

        it "Matches 'Thursday' of this week" $ do
          let res = unTestFixture (convertToDate "Thursday") fixture
          res `shouldBe` (fromGregorian 2017 3 16)

        it "Matches 'Friday' of this week" $ do
          let res = unTestFixture (convertToDate "Friday") fixture
          res `shouldBe` (fromGregorian 2017 3 17)

        it "Matches 'Saturday' of this week" $ do
          let res = unTestFixture (convertToDate "Saturday") fixture
          res `shouldBe` (fromGregorian 2017 3 18)

        it "Matches 'Sunday' of this week" $ do
          let res = unTestFixture (convertToDate "Sunday") fixture
          res `shouldBe` (fromGregorian 2017 3 19)

        it "Throws an error when not a day of the week" $ do
          evaluate (unTestFixture (convertToDate "NotADay") fixture) `shouldThrow` anyErrorCall


      describe "convertStringTypes" $ do
        let day = fromGregorian 2017 3 12
        let date = UTCTime { utctDay = day, utctDayTime = secondsToDiffTime 43200 }
        let kv = SKeyValue $ KVString "due" "today"
        let fixture = def { _getDay = return day
                          , _getUTCTime = return date
                          }
        it "Converts array" $ do
          let res = unTestFixture (convertStringTypes [kv]) fixture
          res `shouldBe` [SKeyValue $ KVDueDate day]
