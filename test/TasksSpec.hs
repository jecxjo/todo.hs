{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TasksSpec where

import Tasks
import Util

import Data.Functor.Identity
import Data.Time (fromGregorian)
import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Test type for verifying MonadDate
newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

unTestM :: TestM a -> a
unTestM (TestM (Identity x)) = x

instance MonadDate TestM where
  getDay = return (fromGregorian 2017 3 13)

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Task Data Types" $ do
    describe "Date" $ do
      it "Shows YYYY-MM-DD with single digit month and day" $ do
        show (Date 2017 2 1) `shouldBe` "2017-02-01"

      it "Shows YYYY-MM-DD with double digit month and day" $ do
        show (Date 2017 12 11) `shouldBe` "2017-12-11"

      it "Two dates are equal" $ do
        (Date 2017 2 1) == (Date 2017 2 1) `shouldBe` True

      it "Two dates are not equal when year is different" $ do
        (Date 2017 2 1) /= (Date 2018 2 1) `shouldBe` True

      it "Two dates are not equal when month is different" $ do
        (Date 2017 2 1) /= (Date 2017 3 1) `shouldBe` True

      it "Two dates are not equal when day is different" $ do
        (Date 2017 2 1) /= (Date 2017 2 2) `shouldBe` True

      it "One date is less than based on year" $ do
        (Date 2017 2 1) `compare` (Date 2018 2 1) `shouldBe` LT

      it "One date is less than based on month" $ do
        (Date 2017 2 1) `compare` (Date 2017 3 1) `shouldBe` LT

      it "One date is less than based on day" $ do
        (Date 2017 2 1) `compare` (Date 2017 2 2) `shouldBe` LT

      it "One date is greater than based on year" $ do
        (Date 2018 2 1) `compare` (Date 2017 2 1) `shouldBe` GT

      it "One date is greater than based on month" $ do
        (Date 2017 3 1) `compare` (Date 2017 2 1) `shouldBe` GT

      it "One date is greater than based on day" $ do
        (Date 2017 2 2) `compare` (Date 2017 2 1) `shouldBe` GT

    describe "KeyValue" $ do
      it "Shows String Key Value Pairs" $ do
        show (KVString "foo" "bar") `shouldBe` "foo:bar"

      it "Shows DueDate Key Value Pairs" $ do
        show (KVDueDate (Date 2017 2 1)) `shouldBe` "due:2017-02-01"


    describe "Task" $ do
      context "Incomplete Task" $ do
        it "Shows Incomplete task with just text" $ do
          show (Incomplete Nothing Nothing [SOther "Example", SOther "task"]) `shouldBe` "Example task"

        it "Shows Incomplete task with date and text" $ do
          show (Incomplete Nothing (Just $ Date 2017 2 1) [SOther "Example", SOther "task"]) `shouldBe` "2017-02-01 Example task"

        it "Shows Incomplete task with priority and text" $ do
          show (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `shouldBe` "(A) Example task"

        it "Shows Incomplete task with Projects" $ do
          show (Incomplete Nothing Nothing [SOther "Example", SOther "task", SProject "ProjectName"]) `shouldBe` "Example task +ProjectName"

        it "Shows Incomplete task with Context" $ do
          show (Incomplete Nothing Nothing [SOther "Example", SOther "task", SContext "ContextName"]) `shouldBe` "Example task @ContextName"

        it "Shows Incomplete task with everything" $ do
          show (Incomplete (Just 'B') (Just $ Date 2017 2 1) [SOther "Example", SOther "task", SProject "ProjectName", SContext "ContextName"]) `shouldBe` "(B) 2017-02-01 Example task +ProjectName @ContextName"

      context "Completed Task" $ do
        it "Show Completed task with everything" $ do
          show (Completed (Date 2017 2 2) (Incomplete (Just 'C') (Just $ Date 2017 2 1) [SOther "Example", SOther "task", SProject "ProjectName", SContext "ContextName"])) `shouldBe` "x 2017-02-02 (C) 2017-02-01 Example task +ProjectName @ContextName"

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
          (Completed (Date 2017 2 2) (Incomplete Nothing Nothing [SOther "Example", SOther "task"])) == (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` False

        it "Two Completed with no priority are equal" $ do
          (Completed (Date 2017 2 2) (Incomplete Nothing Nothing [SOther "Example", SOther "task"])) == (Completed (Date 2017 2 2) (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"])) `shouldBe` True

        it "Two Completed with priorities that match are equal" $ do
          (Completed (Date 2017 2 2) (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"])) == (Completed (Date 2017 2 2) (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"])) `shouldBe` True

        it "Two Incomplete priority is higher than no priority" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `compare` (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` LT

        it "Two Incomplete no priority is lower than priority" $ do
          (Incomplete Nothing Nothing [SOther "Example", SOther "task", SOther "2"]) `compare` (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task"]) `shouldBe` GT

        it "Two Incomplete A > B" $ do
          (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `compare` (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task"]) `shouldBe` LT

        it "Two Incomplete B < A" $ do
          (Incomplete (Just 'B') Nothing [SOther "Example", SOther "task"]) `compare` (Incomplete (Just 'A') Nothing [SOther "Example", SOther "task", SOther "2"]) `shouldBe` GT

      describe "Filters" $ do
        let i1 = Incomplete Nothing Nothing [SOther "Base", SOther "Incomplete", SOther "Task"]
        let i2 = Incomplete Nothing Nothing [SOther "Task", SOther "for", SProject "Foo", SOther "related", SOther "to", SContext "Bar"]
        let i3 = Incomplete Nothing Nothing [SOther "Task", SOther "for", SProject "Foo", SOther "and", SProject "Bar"]
        let i4 = Incomplete Nothing Nothing [SOther "Task", SOther "related", SOther "to", SContext "Foo", SOther "and", SContext "Bar"]
        let c1 = Completed (Date 2017 2 1) i1
        let c2 = Completed (Date 2017 2 1) i2
        let c3 = Completed (Date 2017 2 1) i3
        let c4 = Completed (Date 2017 2 1) i4
        let tasks = [i1,i2,i3,i4,c1,c2,c3,c4]

        it "Filter for only Incomplete" $ do
          onlyPending tasks `shouldBe` [i1,i2,i3,i4]

        it "Filter for only Completed" $ do
          onlyCompleted tasks `shouldBe` [c1,c2,c3,c4]

        it "Filter based on project (single)" $ do
          filterProjects ["Foo"] tasks `shouldBe` [i2,i3,c2,c3]

        it "Filter based on project (multi)" $ do
          filterProjects ["Foo", "Bar"] tasks `shouldBe` [i3,c3]

        it "Filter based on context (single)" $ do
          filterContext ["Bar"] tasks `shouldBe` [i2,i4,c2,c4]

        it "Filter based on context (multi)" $ do
          filterContext ["Foo", "Bar"] tasks `shouldBe` [i4,c4]

      describe "convertToDate" $ do
        it "Matches 'today'" $ do
          let day = unTestM (convertToDate "today")
          show day `shouldBe` "2017-03-13"

        it "Matches 'yesterday'" $ do
          let day = unTestM (convertToDate "yesterday")
          show day `shouldBe` "2017-03-12"

        it "Matches 'tomorrow'" $ do
          let day = unTestM (convertToDate "tomorrow")
          show day `shouldBe` "2017-03-14"

        it "Matches 'Monday' of next week" $ do
          let day = unTestM (convertToDate "Monday")
          show day `shouldBe` "2017-03-20"

        it "Matches 'Tuesday' of this week" $ do
          let day = unTestM (convertToDate "Tuesday")
          show day `shouldBe` "2017-03-14"

        it "Matches 'Wednesday' of this week" $ do
          let day = unTestM (convertToDate "Wednesday")
          show day `shouldBe` "2017-03-15"

        it "Matches 'Thursday' of this week" $ do
          let day = unTestM (convertToDate "Thursday")
          show day `shouldBe` "2017-03-16"

        it "Matches 'Friday' of this week" $ do
          let day = unTestM (convertToDate "Friday")
          show day `shouldBe` "2017-03-17"

        it "Matches 'Saturday' of this week" $ do
          let day = unTestM (convertToDate "Saturday")
          show day `shouldBe` "2017-03-18"

        it "Matches 'Sunday' of this week" $ do
          let day = unTestM (convertToDate "Sunday")
          show day `shouldBe` "2017-03-19"

      describe "convertStringTypes" $ do
        let kv = SKeyValue $ KVString "due" "today"
        it "Converts array" $ do
          let xs = unTestM (convertStringTypes [kv])
          xs `shouldBe` [SKeyValue $ KVDueDate $ Date 2017 3 13]
