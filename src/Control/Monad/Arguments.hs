{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Arguments where

import           Polysemy
import           Polysemy.IO
import           Polysemy.Input
import qualified System.Environment as ENV
import           Data.Text (Text)
import qualified Data.Text as T

data Arguments m a where
  GetArguments :: Arguments m [Text]

makeSem ''Arguments

runArgumentsIO :: Member (Embed IO) r => Sem (Arguments ': r) a -> Sem r a
runArgumentsIO = interpret $ \case
  GetArguments -> fmap (map T.pack) $ embed ENV.getArgs

runArgumentsPure :: [Text] -> Sem (Arguments ': r) a -> Sem r ([Text], a)
runArgumentsPure i
  = runInputConst i
  . reinterpret \case
      GetArguments -> input
