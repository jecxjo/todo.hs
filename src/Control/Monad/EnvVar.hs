{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.EnvVar (
    MonadEnvVar(..)
  ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Writer (WriterT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified System.Environment as IO

class Monad m => MonadEnvVar m where
  lookupEnv :: Text -> m (Maybe Text)

  default lookupEnv :: (MonadTrans t, MonadEnvVar m', m ~ t m') => Text -> m (Maybe Text)
  lookupEnv = lift . lookupEnv

instance MonadEnvVar m => MonadEnvVar (ExceptT e m)
instance MonadEnvVar m => MonadEnvVar (ReaderT r m)
instance MonadEnvVar m => MonadEnvVar (StateT s m)
instance (MonadEnvVar m, Monoid w) => MonadEnvVar (WriterT w m)

instance MonadEnvVar IO where
  lookupEnv name = fmap (fmap T.pack) <$> IO.lookupEnv $ T.unpack name
