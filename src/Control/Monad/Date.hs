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
module Control.Monad.Date where

import           Polysemy
import           Polysemy.IO
import           Data.Time (getCurrentTime, utctDay, UTCTime(..))
import           Data.Time.Calendar (Day(..))

data Date m a where
  GetDay :: Date m Day
  GetUTCTime :: Date m UTCTime

makeSem ''Date

runDateIO :: Member (Embed IO) r => Sem (Date ': r) a -> Sem r a
runDateIO = interpret $ \case
  GetDay -> embed getCurrentTime >>= (return . utctDay)
  GetUTCTime -> embed getCurrentTime
