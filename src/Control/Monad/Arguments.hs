{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.Arguments (
    MonadArguments(..)
  ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Writer (WriterT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified System.Environment as IO

class Monad m => MonadArguments m where
  getArgs :: m [Text]

  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m [Text]
  getArgs = lift getArgs

instance MonadArguments m => MonadArguments (ExceptT e m)
instance MonadArguments m => MonadArguments (ReaderT r m)
instance MonadArguments m => MonadArguments (StateT s m)
instance (MonadArguments m, Monoid w) => MonadArguments (WriterT w m)

instance MonadArguments IO where
  getArgs = map T.pack <$> IO.getArgs
