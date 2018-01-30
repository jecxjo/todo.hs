{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UtilSpec where

import Util (subsetOf, MonadDate(..), getToday, maybeRead, digitCount, showPaddedNumber)

import Data.Functor.Identity
import Data.Time (fromGregorian, toGregorian)
import Test.Hspec (Spec, describe, it, shouldBe)

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
  describe "Utilities" $ do
    describe "subsetOf" $ do
      it "Matches two emtpy lists" $ do
        (([] :: [Int]) `subsetOf` ([] :: [Int])) `shouldBe` True

      it "Matches empty list to filled list" $do
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

    describe "MonadDate" $ do
      it "Returns the Year, Month, Day" $ do
        let today = unTestM $ getToday
        let result = toGregorian today
        result `shouldBe` (2017, 3, 13)

    describe "maybeRead" $ do
      it "Returns Just 1 for \"1\" as type Int" $ do
        let result = (maybeRead "1") :: Maybe Int
        result `shouldBe` (Just 1)

      it "Returns Nothing for \"a\" as type Int" $ do
        let result = (maybeRead "a") :: Maybe Int
        result `shouldBe` Nothing

    describe "digitCount" $ do
      it "Calculates multiple digit positive numbers" $ do
          (map digitCount [0, 1, 12, 123, 1234, 12345, 123456, 1234567, 12345678, 123456789, 1234567890]) `shouldBe` [1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      it "Calculates multiple digit negative numbers" $ do
          (map digitCount [(-1), (-12), (-123), (-1234), (-12345), (-123456), (-1234567), (-12345678), (-123456789), (-1234567890)]) `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    describe "showPaddedNumber" $ do
      it "Adds white space when too small" $ do
        showPaddedNumber 10 123 `shouldBe` "       123"

      it "fits size when too big" $ do
        showPaddedNumber 1 123 `shouldBe` "123"
