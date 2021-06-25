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
module Todo.AppSpec where

import qualified Data.Text as T
import           Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Pos as POS
import           Todo.App

spec :: Spec
spec =
  describe "Todo.App" $ do
    describe "emptyOptions" $ do
      it "is all empty values" $ do
        emptyOptions `shouldBe` Options "" Nothing Nothing Nothing Nothing False False

      it "accesses todoTxtPath" $ do
        todoTxtPath emptyOptions `shouldBe` ""

      it "accesses archiveTxtPath" $ do
        archiveTxtPath emptyOptions `shouldBe` Nothing

      it "accesses reportTxtPath" $ do
        reportTxtPath emptyOptions `shouldBe` Nothing

      it "accesses timeStamp" $ do
        timeStamp emptyOptions `shouldBe` Nothing

      it "accesses autoAccept" $ do
        autoAccept emptyOptions `shouldBe` Nothing

      it "acccess forcedPrompt" $ do
        forcedPrompt emptyOptions `shouldBe` False

      it "access prettyPrinting" $ do
        prettyPrinting emptyOptions `shouldBe` False

    describe "initOptions" $ do
      it "is empty with default path" $ do
        initOptions "todo.txt" `shouldBe` Options "todo.txt" Nothing Nothing Nothing Nothing False False

    describe "renderError" $ do
      it "displays invalid index" $ do
        let res = renderError $ EInvalidIndex 1
        res `shouldBe` "todo: invalid index -- 1"

      it "displays invalid indexes" $ do
        let res = renderError $ EInvalidIndexes [1,2,3]
        res `shouldBe` "todo: invalid index -- 1, 2, 3"

      it "displays invalid argument" $ do
        let res = renderError $ EInvalidArg "badarg"
        res `shouldBe` "todo: invalid argument -- 'badarg'"

      it "displays an IO Exception" $ do
        let res = renderError $ EIOError $ userError "Unit Testing"
        res `shouldBe` "todo: user error (Unit Testing)"

      it "displays misc error" $ do
        let res = renderError $ EMiscError "Misc Error"
        res `shouldBe` "todo: Misc Error"

      it "displays Parser Error" $ do
        let eMsg = PE.Message $ T.unpack "Error Message"
        let ePos = POS.newPos (T.unpack "FileName") 1 2
        let err = PE.newErrorMessage eMsg ePos
        let res = renderError $ EParseError err
        res `shouldBe` "todo: parse error(s) -- \"FileName\" (line 1, column 2) Error Message"

      it "displays multi lined Parser Error" $ do
        let eMsg = PE.Message $ T.unpack "Error Message"
        let ePos = POS.newPos (T.unpack "FileName") 1 2
        let err = PE.newErrorMessage eMsg ePos
        let err' = PE.addErrorMessage (PE.Expect $ T.unpack "Expected Token") err
        let err'' = PE.addErrorMessage (PE.UnExpect $ T.unpack "Unexpected Token") err'
        let res = renderError $ EParseError err''
        res `shouldBe` "todo: parse error(s) -- \"FileName\" (line 1, column 2) Unexpected Token\nExpected Token\nError Message"

      it "displays Short Circuit" $ do
        let res = renderError $ EShortCircuit "Testing"
        res `shouldBe` "Testing"
