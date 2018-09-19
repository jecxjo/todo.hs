{-# LANGUAGE OverloadedStrings #-}
module Todo.Parser.AtomsSpec where

import qualified Todo.Parser.Atoms as A

import Text.Parsec.Prim (parse)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Atom Parsers" $ do
    describe "White Space" $ do
      it "Matches ' ' as whiteSpace" $ do
        parse A.whiteSpace "" " " `shouldBe` Right ()
