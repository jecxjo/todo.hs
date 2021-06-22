{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Date where

import Control.Monad.Freer
  ( type (~>)
  , Eff
  , LastMember
  , Member
  , interpret
  , interpretM
  , send
  )
import Control.Monad.Freer.Reader (Reader, asks, ask)
import Data.Time (getCurrentTime, utctDay, UTCTime(..))
import Data.Time.Calendar (Day(..))

data Date r where
  GetDay :: Date Day
  GetUTCTime :: Date UTCTime

getDay :: Member Date effs => Eff effs Day
getDay = send GetDay

getUTCTime :: Member Date effs => Eff effs UTCTime
getUTCTime = send GetUTCTime

interpretIO :: (LastMember IO effs, Member IO effs) => Eff (Date ': effs) ~> Eff effs
interpretIO = interpretM (\case
                            GetDay -> utctDay <$> getCurrentTime
                            GetUTCTime -> getCurrentTime)

interpretPure :: Member (Reader (Day, UTCTime)) effs => Eff (Date ': effs) ~> Eff effs
interpretPure = interpret (\case
                              GetDay -> ask >>= fst
                              GetUTCTime -> ask >>= snd)

