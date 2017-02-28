module TasksSpec where

import Tasks

import Test.Hspec (Spec, describe, context, it, shouldBe)

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

    describe "Task" $ do
      context "Incomplete Task" $ do
        it "Shows Incomplete task with just text" $ do
          show (Incomplete Nothing Nothing [] [] "Example task") `shouldBe` "Example task"

        it "Shows Incomplete task with date and text" $ do
          show (Incomplete Nothing (Just $ Date 2017 2 1) [] [] "Example task") `shouldBe` "2017-02-01 Example task"

        it "Shows Incomplete task with priority and text" $ do
          show (Incomplete (Just 'A') Nothing [] [] "Example task") `shouldBe` "(A) Example task"

        it "Shows Incomplete task with Projects" $ do
          show (Incomplete Nothing Nothing ["ProjectName"] [] "Example task +ProjectName") `shouldBe` "Example task +ProjectName"

        it "Shows Incomplete task with Context" $ do
          show (Incomplete Nothing Nothing [] ["ContextName"] "Example task @ContextName") `shouldBe` "Example task @ContextName"

        it "Shows Incomplete task with everything" $ do
          show (Incomplete (Just 'B') (Just $ Date 2017 2 1) ["ProjectName"] ["ContextName"] "Example task +ProjectName @ContextName") `shouldBe` "(B) 2017-02-01 Example task +ProjectName @ContextName"

      context "Completed Task" $ do
        it "Show Completed task with everything" $ do
          show (Completed (Date 2017 2 2) (Incomplete (Just 'C') (Just $ Date 2017 2 1) ["ProjectName"] ["ContextName"] "Example task +ProjectName @ContextName")) `shouldBe` "x 2017-02-02 (C) 2017-02-01 Example task +ProjectName @ContextName"

      context "Compare Tasks" $ do
        it "Two Incomplete with no priority are equal" $ do
          (Incomplete Nothing Nothing [] [] "Example task") == (Incomplete Nothing Nothing [] [] "Example task 2") `shouldBe` True

        it "Two Incomplete with priority and none are not equal" $ do
          (Incomplete (Just 'A') Nothing [] [] "Example task") == (Incomplete Nothing Nothing [] [] "Example task 2") `shouldBe` False

        it "Two Incomplete with priority and none are not equal" $ do
          (Incomplete Nothing Nothing [] [] "Example task 2") == (Incomplete (Just 'A') Nothing [] [] "Example task") `shouldBe` False

        it "Two Incomplete with priorities that match are equal" $ do
          (Incomplete (Just 'A') Nothing [] [] "Example task") == (Incomplete (Just 'A') Nothing [] [] "Example task 2") `shouldBe` True

        it "Two Incomplete with priorities that don't match are not equal" $ do
          (Incomplete (Just 'A') Nothing [] [] "Example task") == (Incomplete (Just 'B') Nothing [] [] "Example task 2") `shouldBe` False

        it "Completed and Incomplete are not equal" $ do
          (Completed (Date 2017 2 2) (Incomplete Nothing Nothing [] [] "Example task")) == (Incomplete Nothing Nothing [] [] "Example task 2") `shouldBe` False

        it "Two Completed with no priority are equal" $ do
          (Completed (Date 2017 2 2) (Incomplete Nothing Nothing [] [] "Example task")) == (Completed (Date 2017 2 2) (Incomplete Nothing Nothing [] [] "Example task 2")) `shouldBe` True

        it "Two Completed with priorities that match are equal" $ do
          (Completed (Date 2017 2 2) (Incomplete (Just 'A') Nothing [] [] "Example task")) == (Completed (Date 2017 2 2) (Incomplete (Just 'A') Nothing [] [] "Example task 2")) `shouldBe` True

        it "Two Incomplete priority is higher than no priority" $ do
          (Incomplete (Just 'A') Nothing [] [] "Example task") `compare` (Incomplete Nothing Nothing [] [] "Example task 2") `shouldBe` GT

        it "Two Incomplete no priority is lower than priority" $ do
          (Incomplete Nothing Nothing [] [] "Example task 2") `compare` (Incomplete (Just 'A') Nothing [] [] "Example task") `shouldBe` LT

        it "Two Incomplete A > B" $ do
          (Incomplete (Just 'A') Nothing [] [] "Example task 2") `compare` (Incomplete (Just 'B') Nothing [] [] "Example task") `shouldBe` GT

        it "Two Incomplete B < A" $ do
          (Incomplete (Just 'B') Nothing [] [] "Example task") `compare` (Incomplete (Just 'A') Nothing [] [] "Example task 2") `shouldBe` LT

      describe "Filters" $ do
        let i1 = Incomplete Nothing Nothing [] [] "Base Incomplete Task"
        let i2 = Incomplete Nothing Nothing ["Foo"] ["Bar"] "Task for +Foo related to @Bar"
        let i3 = Incomplete Nothing Nothing ["Foo","Bar"] [] "Task for +Foo and +Bar"
        let i4 = Incomplete Nothing Nothing [] ["Foo","Bar"] "Task related to @Foo and @Bar"
        let c1 = Completed (Date 2017 2 1) i1
        let c2 = Completed (Date 2017 2 1) i2
        let c3 = Completed (Date 2017 2 1) i3
        let c4 = Completed (Date 2017 2 1) i4
        let tasks = [i1,i2,i3,i4,c1,c2,c3,c4]

        it "Filter for only Incomplete" $ do
          onlyPending tasks `shouldBe` [i1,i2,i3,i4]

        it "Filter for only Completedd" $ do
          onlyCompleted tasks `shouldBe` [c1,c2,c3,c4]

        it "Filter based on project (single)" $ do
          filterProjects ["Foo"] tasks `shouldBe` [i2,i3,c2,c3]

        it "Filter based on project (multi)" $ do
          filterProjects ["Foo", "Bar"] tasks `shouldBe` [i3,c3]

        it "Filter based on context (single)" $ do
          filterContext ["Bar"] tasks `shouldBe` [i2,i4,c2,c4]

        it "Filter based on context (multi)" $ do
          filterContext ["Foo", "Bar"] tasks `shouldBe` [i4,c4]
