{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UtilSpec where

import Util (subsetOf, MonadDate(..), getToday)

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

