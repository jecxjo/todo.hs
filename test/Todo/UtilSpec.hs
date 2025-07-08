{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Todo.UtilSpec where

import qualified Data.List as L
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Todo.Util

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Utilities" $ do
    describe "subsetOf" $ do
      it "Matches two emtpy lists" $ do
        (([] :: [Int]) `subsetOf` ([] :: [Int])) `shouldBe` True

      it "Matches empty list to filled list" $ do
        (([] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` True

      it "Matches single item list" $ do
        (([2] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` True

      it "Matches multi item list" $ do
        (([1,3] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` True

      it "Matches same lists" $ do
        (([1,2,3] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` True

      it "Does not match single item list" $ do
        (([4] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` False

      it "Does not match multi item list" $ do
        (([4,5] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` False

      it "Does not match partial matching list with extra" $ do
        (([2,3,4] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` False

      it "Does not match matching list with extra" $ do
        (([1,2,3,4] :: [Int]) `subsetOf` ([1,2,3] :: [Int])) `shouldBe` False

      it "Does not match subset of empty list" $ do
        (([2] :: [Int]) `subsetOf` ([] :: [Int])) `shouldBe` False

    describe "maybeRead" $ do
      it "Returns Just 1 for \"1\" as type Int" $ do
        let result = (maybeRead "1") :: Maybe Int
        result `shouldBe` (Just 1)

      it "Returns Nothing for \"a\" as type Int" $ do
        let result = (maybeRead "a") :: Maybe Int
        result `shouldBe` Nothing

    describe "digitCount" $ do
      it "Calculates multiple digit positive numbers" $ do
          (map digitCount [0, 1, 12, 123, 1234, 12345, 123456, 1234567, 12345678, 123456789, 1234567890])  `shouldBe` [1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      it "Calculates multiple digit negative numbers" $ do
          (map digitCount [(-1), (-12), (-123), (-1234), (-12345), (-123456), (-1234567), (-12345678), (-123456789), (-1234567890)]) `shouldBe` ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Integer])

    describe "showPaddedNumber" $ do
      it "Adds white space when too small" $ do
        showPaddedNumber ' ' 10 123 `shouldBe` "       123"

      it "fits size when too big" $ do
        showPaddedNumber ' ' 1 123 `shouldBe` ("123" :: String)

      it "Adds '0' when too small" $ do
        showPaddedNumber '0' 10 123 `shouldBe` "0000000123"

    describe "notEmpty" $ do
      let fn :: [Int] -> Int
          fn = L.maximum

      it "Returns value when empty" $ do
        (notEmpty (-1) fn ([] :: [Int])) `shouldBe` (-1)

      it "Returns computation when not empty" $ do
        (notEmpty (-1) fn ([1,2,3] :: [Int])) `shouldBe` 3

    describe "maybeFilter" $ do
      it "returns nothing when nothing is filtered" $ do
        maybeFilter even [1,3,5] `shouldBe` (Nothing :: Maybe [Int])

      it "returns nothing when nothing is filtered" $ do
        maybeFilter even [2,3,4] `shouldBe` (Just [2,4] :: Maybe [Int])

    describe "maybeToEither" $ do
      it "returns left on nothing" $ do
        maybeToEither "error case" (Nothing :: Maybe Int) `shouldBe` (Left "error case" :: Either String Int)

      it "returns left on nothing" $ do
        maybeToEither "error case" (Just 1 :: Maybe Int) `shouldBe` (Right 1 :: Either String Int)

    describe "replace" $ do
      it "returns an empty list when an empty list" $ do
        replace 1 2 [] `shouldBe` ([] :: [Int])

      it "returns a list with no changes when no matches" $ do
        replace 1 2 [3,4,5] `shouldBe` ([3,4,5] :: [Int])

      it "returns a list with no changes when no matches" $ do
        replace 1 2 [1,4,1] `shouldBe` ([2,4,2] :: [Int])

