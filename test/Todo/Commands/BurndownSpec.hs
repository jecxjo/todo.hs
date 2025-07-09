{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Todo.Commands.BurndownSpec where

import           Data.Time.Calendar (fromGregorian)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Todo.Commands.Burndown
import           Todo.Tasks (Task(..))

-- | Rrequired for auto-discovery
spec :: Spec
spec =
    describe "Commands.Burndown" $ do
        describe "calculateActiveTasks" $ do
            it "Returns list of zeros when no tasks" $ do
                let result = calculateActiveTasks [] (fromGregorian 2023 1 1) (fromGregorian 2023 1 3)
                result `shouldBe` [ (fromGregorian 2023 01 01, 0)
                                  , (fromGregorian 2023 01 02, 0) 
                                  , (fromGregorian 2023 01 03, 0) 
                                  ]

            it "Returns correct active tasks for a single day" $ do
                let tasks = [Incomplete Nothing (Just $ fromGregorian 2023 1 5) []]
                let result = calculateActiveTasks tasks (fromGregorian 2023 1 5) (fromGregorian 2023 1 5)
                result `shouldBe` [(fromGregorian 2023 1 5, 1)]

            it "Counts active tasks across multiple days" $ do
                let tasks = [ Incomplete Nothing (Just $ fromGregorian 2023 1 5) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 6) []
                            ]
                let result = calculateActiveTasks tasks (fromGregorian 2023 1 4) (fromGregorian 2023 1 7)
                result `shouldBe` [(fromGregorian 2023 1 4, 0),
                                   (fromGregorian 2023 1 5, 1),
                                   (fromGregorian 2023 1 6, 2),
                                   (fromGregorian 2023 1 7, 2)]

            it "Counts completed tasks correctly" $ do
                let tasks = [ Completed (fromGregorian 2023 1 7) (Incomplete Nothing (Just $ fromGregorian 2023 1 5) [])
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 6) []
                            ]
                let result = calculateActiveTasks tasks (fromGregorian 2023 1 4) (fromGregorian 2023 1 8)
                result `shouldBe` [ (fromGregorian 2023 1 4, 0)
                                  , (fromGregorian 2023 1 5, 1)
                                  , (fromGregorian 2023 1 6, 2)
                                  , (fromGregorian 2023 1 7, 2)
                                  , (fromGregorian 2023 1 8, 1)
                                  ]

        describe "calculateActiveTasksNormalized" $ do
            it "Normalizes the counts to be proportional to the expected output size" $ do
                let tasks = [ Incomplete Nothing (Just $ fromGregorian 2023 1 5) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 5) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 6) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 6) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 6) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 6) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 7) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 7) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 7) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 8) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 8) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 8) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 8) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 8) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 9) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 9) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 9) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 9) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 9) []
                            , Incomplete Nothing (Just $ fromGregorian 2023 1 9) []
                            ]
                let result = calculateActiveTasksNormalized 10 tasks (fromGregorian 2023 1 4) (fromGregorian 2023 1 10)
                result `shouldBe` [ (fromGregorian 2023 1 4, 0)
                                  , (fromGregorian 2023 1 5, 1)
                                  , (fromGregorian 2023 1 6, 3)
                                  , (fromGregorian 2023 1 7, 4)
                                  , (fromGregorian 2023 1 8, 7)
                                  , (fromGregorian 2023 1 9, 10)
                                  , (fromGregorian 2023 1 10, 10)
                                  ]
