{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Process (
    MonadProcess(..)
  ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Writer (WriterT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           System.Exit (ExitCode(..))
import qualified System.Process as P

class Monad m => MonadProcess m where
  -- | Run a process giving path, env vars, command and args.
  runProcess :: String -> [(String, String)] -> String -> [String] -> m Int

  default runProcess :: (MonadTrans t, MonadProcess m', m ~ t m') => String -> [(String, String)] -> String -> [String] -> m Int
  runProcess path envVars cmd args = lift $ runProcess path envVars cmd args

instance MonadProcess m => MonadProcess (ExceptT e m)
instance MonadProcess m => MonadProcess (ReaderT r m)
instance MonadProcess m => MonadProcess (StateT s m)
instance (MonadProcess m, Monoid w) => MonadProcess (WriterT w m)

instance MonadProcess IO where
  runProcess path envVars cmd args = do
    (_, _, _, ph) <- P.createProcess (P.proc cmd args){ P.cwd = Just path
                                                      , P.env = Just envVars
                                                      }
    exitCode <- P.waitForProcess ph
    case exitCode of
        ExitSuccess -> return 0
        ExitFailure n -> return n

