{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Arguments where

import Control.Monad.Freer
  ( type (~>)
  , Eff
  , LastMember
  , Member
  , interpret
  , interpretM
  , send
  )
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Environment as IO

data Arguments r where
  GetArgs :: Arguments [Text]

getArgs :: Member Arguments effs => Eff effs [Text]
getArgs = send GetArgs

interpretIO :: (LastMember IO effs, Member IO effs) => Eff (Arguments ': effs) ~> Eff effs
interpretIO = interpretM (\case
                            GetArgs -> map T.pack <$> IO.getArgs)

interpretPure :: Member (Reader [Text]) effs => Eff (Arguments ': effs) ~> Eff effs
interpretPure = interpret (\case
                              GetArgs -> ask)
