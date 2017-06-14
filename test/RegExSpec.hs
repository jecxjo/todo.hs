{-# LANGUAGE OverloadedStrings #-}
module RegExSpec where

import RegEx (matchGen, swapGen, swapAllGen)

import Test.Hspec (Spec, describe, it, shouldBe)

-- |Required for auto-discovery
spec :: Spec
spec =
  describe "RegEx" $ do
    describe "matchGen" $ do
      describe "Emtpy RegEx" $ do
        let fn = matchGen ""

        it "Matches with empty string" $ do
          fn "" `shouldBe` True

        it "Matches with non-empty string" $ do
          fn "testing" `shouldBe` True

      describe "Non-empty RegEx" $ do
        let fn = matchGen "foo"

        it "Matches exact match string" $ do
          fn "foo" `shouldBe` True

        it "Doesn't match string with nothing matching" $ do
          fn "bar" `shouldBe` False

        it "Matches first of multi word" $ do
          fn "foo bar baz" `shouldBe` True

        it "Matches middle of multi word" $ do
          fn "bar foo baz" `shouldBe` True

        it "Matches end of multi word" $ do
          fn "bar baz foo" `shouldBe` True

    describe "swapGen" $ do
      it "works with empty string" $ do
        let swap = swapGen "foo" "bar"
        let results = swap ""
        results `shouldBe` ""

      it "works with empty re and replace strings" $ do
        let swap = swapGen "" ""
        let results = swap "Foo"
        results `shouldBe` "Foo"

      it "leaves non-matching strings unchanged" $ do
        let swap = swapGen "foo" "bar"
        let results = swap "baz"
        results `shouldBe` "baz"

      it "replaces first instance of match" $ do
        let swap = swapGen "foo" "bar"
        let results = swap "foo foo"
        results `shouldBe` "bar foo"

      it "replaces and uses submatch" $ do
        let swap = swapGen "ba(r|z)" "\\1ap"
        let results = swap "baz"
        results `shouldBe` "zap"

    describe "swapAllGen" $ do
      it "works with empty string" $ do
        let swap = swapAllGen "foo" "bar"
        let results = swap ""
        results `shouldBe` ""

      it "works with empty re and replace strings" $ do
        let swap = swapAllGen "" ""
        let results = swap "Foo"
        results `shouldBe` "Foo"

      it "leaves non-matching strings unchanged" $ do
        let swap = swapAllGen "foo" "bar"
        let results = swap "baz"
        results `shouldBe` "baz"

      it "replaces all instance of match" $ do
        let swap = swapAllGen "foo" "bar"
        let results = swap "foo foo"
        results `shouldBe` "bar bar"

      it "replaces and uses submatch" $ do
        let swap = swapAllGen "ba(r|z)" "\\1ap"
        let results = swap "baz foo bar"
        results `shouldBe` "zap foo rap"

