{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Parser
import Tasks

import Text.Parsec.Prim (parse)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Parsers" $ do
    describe "Priority" $ do
      it "Matches (A) as Priority A" $ do
        parse priority "" "(A)" `shouldBe` Right ('A' :: Priority)

      it "Doesn't match A as Priority A" $ do
        parse priority "" "A" `shouldNotBe` Right ('A' :: Priority)

    describe "Date" $ do
      it "Matches 2007-01-02" $ do
        parse Parser.date "" "2007-01-02" `shouldBe` Right (Tasks.Date 2007 1 2)

      it "Matches 2007-1-2" $ do
        parse Parser.date "" "2007-1-2" `shouldBe` Right (Tasks.Date 2007 1 2)

      it "Matches 07-1-2" $ do
        parse Parser.date "" "07-1-2" `shouldBe` Right (Tasks.Date 2007 1 2)

      it "Doesn't match 7-1-2" $ do
        parse Parser.date "" "7-1-2" `shouldNotBe` Right (Tasks.Date 2007 1 2)

    describe "Project" $ do
      it "Matches +ProjectName" $ do
        parse Parser.project "" "+ProjectName" `shouldBe` Right (EProject "ProjectName")

      it "Doesn't match ProjectName" $ do
        parse Parser.project "" "ProjectName" `shouldNotBe` Right (EProject "ProjectName")

      it "Doesn't match @ProjectName" $ do
        parse Parser.project "" "@ProjectName" `shouldNotBe` Right (EProject "ProjectName")

    describe "Context" $ do
      it "Matches @ContextString" $ do
        parse Parser.context "" "@ContextString" `shouldBe` Right (EContext "ContextString")

      it "Doesn't match ContextString" $ do
        parse Parser.context "" "ContextString" `shouldNotBe` Right (EContext "ContextString")

      it "Doesn't match +ContextString" $ do
        parse Parser.context "" "+ContextString" `shouldNotBe` Right (EContext "ContextString")

    describe "Other Strings" $ do
      it "Matches Other123" $ do
        parse Parser.other "" "Other123" `shouldBe` Right (EOther "Other123")

    describe "String Type Parser" $ do
      it "Matches +FooBar as Project" $ do
        parse Parser.stringTypes "" "+FooBar" `shouldBe` Right (EProject "FooBar")

      it "Matches @FooBar as Context" $ do
        parse Parser.stringTypes "" "@FooBar" `shouldBe` Right (EContext "FooBar")

      it "Matches FooBar as other string" $ do
        parse Parser.stringTypes "" "FooBar" `shouldBe` Right (EOther "FooBar")

    describe "A lot of string types" $ do
      it "Matches: Example @Haskell @Parsing task for +TodoTxt +Blog post" $ do
        parse Parser.lots "" "Example @Haskell @Parsing task for +TodoTxt +Blog post"
          `shouldBe` Right [
                             (EOther "Example")
                           , (EContext "Haskell")
                           , (EContext "Parsing")
                           , (EOther "task")
                           , (EOther "for")
                           , (EProject "TodoTxt")
                           , (EProject "Blog")
                           , (EOther "post")
                           ]
    describe "Tasks" $ do
      let i1 = Incomplete Nothing Nothing ["Project"] ["Context"] "Basic task @Context and +Project"
      let i2 = Incomplete Nothing (Just $ Date 2017 2 1) ["Project"] ["Context"] "Basic task @Context and +Project"
      let i3 = Incomplete (Just 'A') Nothing ["Project"] ["Context"] "Basic task @Context and +Project"
      let i4 = Incomplete (Just 'A') (Just $ Date 2017 2 1) ["Project"] ["Context"] "Basic task @Context and +Project"
      let c1 = Completed (Date 2017 3 1) i1
      let c2 = Completed (Date 2017 3 1) i2
      let c3 = Completed (Date 2017 3 1) i3
      let c4 = Completed (Date 2017 3 1) i4

      describe "Incomplete Task" $ do
        it "Basic, no date or priority" $ do
          parse Parser.incompleteTask "" "Basic task @Context and +Project"
            `shouldBe` Right i1

        it "Date and no priority" $ do
          parse Parser.incompleteTask "" "2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right i2

        it "Priority and no date" $ do
          parse Parser.incompleteTask "" "(A) Basic task @Context and +Project"
            `shouldBe` Right i3

        it "Date and priority" $ do
          parse Parser.incompleteTask "" "(A) 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right i4

      describe "Completed Task" $ do
        it "Basic, no date or priority" $ do
          parse Parser.completedTask "" "x 2017-03-01 Basic task @Context and +Project"
            `shouldBe` Right c1

        it "Date and no priority" $ do
          parse Parser.completedTask "" "x 2017-03-01 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right c2

        it "Priority and no date" $ do
          parse Parser.completedTask "" "x 2017-03-01 (A) Basic task @Context and +Project"
            `shouldBe` Right c3

        it "Date and priority" $ do
          parse Parser.completedTask "" "x 2017-03-01 (A) 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right c4

      describe "Both Tasks" $ do
        it "Matches Full Incomplete Task" $ do
          parse Parser.task "" "(A) 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right i4

        it "Matches Full Complete Task" $ do
          parse Parser.task "" "x 2017-03-01 (A) 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right c4
