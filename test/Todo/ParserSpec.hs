{-# LANGUAGE OverloadedStrings #-}
module Todo.ParserSpec where

import qualified Todo.Parser as P
import qualified Todo.Tasks as T

import Data.Either (isLeft)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..))
import Text.Parsec.Prim (parse)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Parsers" $ do
    describe "Priority" $ do
      it "Matches (A) as Priority A" $ do
        parse P.priority "" "(A)" `shouldBe` Right ('A' :: T.Priority)

      it "Doesn't match A as Priority A" $ do
        isLeft (parse P.priority "" "A") `shouldBe` True

    describe "Date" $ do
      it "Matches 2007-01-02" $ do
        parse P.date "" "2007-01-02" `shouldBe` Right (fromGregorian 2007 1 2)

      it "Matches 2007-1-2" $ do
        parse P.date "" "2007-1-2" `shouldBe` Right (fromGregorian 2007 1 2)

      it "Matches 07-1-2" $ do
        parse P.date "" "07-1-2" `shouldBe` Right (fromGregorian 2007 1 2)

      it "Doesn't match 7-1-2" $ do
        isLeft (parse P.date "" "7-1-2") `shouldBe` True

      it "Doesn't match bad month" $ do
        isLeft (parse P.date "" "2007-13-02") `shouldBe` True

      it "Doesn't match bad day" $ do
        isLeft (parse P.date "" "2007-10-32") `shouldBe` True

      it "Matches leap year" $ do
        parse P.date "" "2012-02-29" `shouldBe` Right (fromGregorian 2012 2 29)
        isLeft (parse P.date "" "2013-02-29") `shouldBe` True

    describe "Project" $ do
      it "Matches +ProjectName" $ do
        parse P.project "" "+ProjectName" `shouldBe` Right (T.SProject "ProjectName")

      it "Doesn't match ProjectName" $ do
        isLeft (parse P.project "" "ProjectName") `shouldBe` True

      it "Doesn't match @ProjectName" $ do
        isLeft (parse P.project "" "@ProjectName") `shouldBe` True

      it "Matches +ProjectName.SubProjectName" $ do
        parse P.project "" "+ProjectName.SubProjectName" `shouldBe` Right (T.SProject "ProjectName.SubProjectName")

      it "Matches +ProjectName-SubProjectName" $ do
        parse P.project "" "+ProjectName-SubProjectName" `shouldBe` Right (T.SProject "ProjectName-SubProjectName")

      it "Doesn't match +ProjectName..SubProjectName" $ do
        isLeft (parse P.project "" "+ProjectName..SubProjectName") `shouldBe` True

      it "Doesn't match +ProjectName--SubProjectName" $ do
        isLeft (parse P.project "" "+ProjectName--SubProjectName") `shouldBe` True

    describe "Context" $ do
      it "Matches @ContextString" $ do
        parse P.context "" "@ContextString" `shouldBe` Right (T.SContext "ContextString")

      it "Doesn't match ContextString" $ do
        isLeft (parse P.context "" "ContextString") `shouldBe` True

      it "Doesn't match +ContextString" $ do
        isLeft (parse P.context "" "+ContextString") `shouldBe` True

      it "Matches @foo+baz@bar-quux.com" $ do
        parse P.context "" "@foo+baz@bar-quux.com" `shouldBe` Right (T.SContext "foo+baz@bar-quux.com")

      it "Matches @@call" $ do
        parse P.context "" "@@call" `shouldBe` Right (T.SContext "@call")

    describe "Key Value Pair" $ do
      it "Matches foo:bar" $ do
        parse P.keyvalue "" "foo:bar" `shouldBe` Right (T.SKeyValue $ T.KVString "foo" "bar")

      it "Matches count:5" $ do
        parse P.keyvalue "" "count:5" `shouldBe` Right (T.SKeyValue $ T.KVString "count" "5")

      it "Matches due:2017-03-13" $ do
        parse P.keyvalue "" "due:2017-03-13" `shouldBe` Right (T.SKeyValue $ T.KVDueDate (fromGregorian 2017 3 13))

      it "Matches t:2018-05-03" $ do
        parse P.keyvalue "" "t:2018-05-03" `shouldBe` Right (T.SKeyValue $ T.KVThreshold (fromGregorian 2018 5 3))

      it "Matches at:0000" $ do
        parse P.keyvalue "" "at:0000" `shouldBe` Right (T.SKeyValue $ T.KVAt (TimeOfDay 0 0 0))

      it "Matches quoted strings test:\"value\"" $ do
        parse P.keyvalue "" "test:\"value\"" `shouldBe` Right (T.SKeyValue $ T.KVString "test" "\"value\"")

      it "Matches non-alphanumeric in value" $ do
        parse P.keyvalue "" "test:`~!@#$%^&*()_+[]{};',./<>?" `shouldBe` Right (T.SKeyValue $ T.KVString "test" "`~!@#$%^&*()_+[]{};',./<>?")

    describe "Other Strings" $ do
      it "Matches Other123" $ do
        parse P.other "" "Other123" `shouldBe` Right (T.SOther "Other123")

    describe "String Type Parser" $ do
      it "Matches +FooBar as Project" $ do
        parse P.stringTypes "" "+FooBar" `shouldBe` Right (T.SProject "FooBar")

      it "Matches @FooBar as Context" $ do
        parse P.stringTypes "" "@FooBar" `shouldBe` Right (T.SContext "FooBar")

      it "Matches FooBar as other string" $ do
        parse P.stringTypes "" "FooBar" `shouldBe` Right (T.SOther "FooBar")

    describe "A lot of string types" $ do
      it "Matches: Example @Haskell @Parsing task for +TodoTxt +Blog post" $ do
        parse P.lots "" "Example @Haskell @Parsing task for +TodoTxt +Blog post due:today"
          `shouldBe` Right [
                             (T.SOther "Example")
                           , (T.SContext "Haskell")
                           , (T.SContext "Parsing")
                           , (T.SOther "task")
                           , (T.SOther "for")
                           , (T.SProject "TodoTxt")
                           , (T.SProject "Blog")
                           , (T.SOther "post")
                           , (T.SKeyValue (T.KVString "due" "today"))
                           ]
    describe "Tasks" $ do
      let i1 = T.Incomplete Nothing Nothing [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let i2 = T.Incomplete Nothing (Just $ fromGregorian 2017 2 1) [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let i3 = T.Incomplete (Just 'A') Nothing [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let i4 = T.Incomplete (Just 'A') (Just $ fromGregorian 2017 2 1) [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let c1 = T.Completed Nothing Nothing (Just $ fromGregorian 2017 3 1) [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let c2 = T.Completed Nothing (Just $ fromGregorian 2017 3 1) (Just $ fromGregorian 2017 2 1) [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let c3 = T.Completed (Just 'A') Nothing (Just $ fromGregorian 2017 3 1) [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]
      let c4 = T.Completed (Just 'A') (Just $ fromGregorian 2017 3 1) (Just $ fromGregorian 2017 2 1) [T.SOther "Basic", T.SOther "task", T.SContext "Context", T.SOther "and", T.SProject "Project"]

      describe "Incomplete Task" $ do
        it "Basic, no date or priority" $ do
          parse P.incompleteTask "" "Basic task @Context and +Project"
            `shouldBe` Right i1

        it "Date and no priority" $ do
          parse P.incompleteTask "" "2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right i2

        it "Priority and no date" $ do
          parse P.incompleteTask "" "(A) Basic task @Context and +Project"
            `shouldBe` Right i3

        it "Date and priority" $ do
          parse P.incompleteTask "" "(A) 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right i4

      describe "Completed Task" $ do
        it "Basic, no date or priority" $ do
          parse P.completedTask "" "x 2017-03-01 Basic task @Context and +Project"
            `shouldBe` Right c1

        it "Date and no priority" $ do
          parse P.completedTask "" "x 2017-03-01 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right c2

        it "Priority and no date" $ do
          parse P.completedTask "" "x (A) 2017-03-01 Basic task @Context and +Project"
            `shouldBe` Right c3

        it "Date and priority" $ do
          parse P.completedTask "" "x (A) 2017-03-01 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right c4

      describe "Both Tasks" $ do
        it "Matches Full Incomplete Task" $ do
          parse P.task "" "(A) 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right i4

        it "Matches Full Complete Task" $ do
          parse P.task "" "x (A) 2017-03-01 2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right c4

      describe "Tasks" $ do
        it "Empty string returns empty list" $ do
          parse P.tasks "" "" `shouldBe` Right []

        it "Multiple newline string returns empty list" $ do
          parse P.tasks "" "\n\n\n" `shouldBe` Right []

        it "Multiple valid lines" $ do
          parse P.tasks "" "Basic task @Context and +Project\n2017-02-01 Basic task @Context and +Project"
            `shouldBe` Right [i1,i2]

        it "Multiple valid lines with newlines interlaced" $ do
          parse P.tasks "" "\n\n\n\nBasic task @Context and +Project\n\n\n2017-02-01 Basic task @Context and +Project\n\n\n"
            `shouldBe` Right [i1,i2]
