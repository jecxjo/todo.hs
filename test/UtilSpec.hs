module UtilSpec where

import Util (subsetOf)

import Test.Hspec (Spec, describe, it, shouldBe)

-- | Required for auto-discovery
spec :: Spec
spec =
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
