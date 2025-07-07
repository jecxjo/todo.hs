{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Date (MonadDate(..)) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Writer (WriterT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Time (getCurrentTime, utctDay, UTCTime(..))
import           Data.Time.Calendar (Day(..))

class Monad m => MonadDate m where
  getDay :: m Day
  getUTCTime :: m UTCTime

  default getDay :: (MonadTrans t, MonadDate m', m ~ t m') => m Day
  getDay = lift getDay

  default getUTCTime :: (MonadTrans t, MonadDate m', m ~ t m') => m UTCTime
  getUTCTime = lift getUTCTime

instance MonadDate m => MonadDate (ExceptT e m)
instance MonadDate m => MonadDate (ReaderT r m)
instance MonadDate m => MonadDate (StateT s m)
instance (MonadDate m, Monoid w) => MonadDate (WriterT w m)

instance MonadDate IO where
  getDay = utctDay <$> getCurrentTime
  getUTCTime = getCurrentTime
