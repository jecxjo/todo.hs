{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.FileSystem (
    MonadFileSystem(..)
  ) where

import qualified Control.Exception as E
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer (WriterT)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (listDirectory)

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns it contents. If an error
  -- occurs, the method returns an error.
  readFileSafe ::  Text -> m (Either E.IOException Text)

  default readFileSafe :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m (Either E.IOException Text)
  readFileSafe = lift . readFileSafe

  -- | Writes a file at the given path. If an error occurs, the method returns
  -- an error.
  writeFileSafe :: (E.Exception e) => Text -> Text -> m (Either e ())

  default writeFileSafe :: (MonadTrans t, MonadFileSystem m', m ~ t m', E.Exception e) => Text -> Text -> m (Either e ())
  writeFileSafe path str = lift $ writeFileSafe path str

  -- | Append a file at the given path. If an error occurs, the method returns
  -- an error.
  appendFileSafe :: (E.Exception e) => Text -> Text -> m (Either e ())

  default appendFileSafe :: (MonadTrans t, MonadFileSystem m', m ~ t m', E.Exception e) => Text -> Text -> m (Either e ())
  appendFileSafe path str = lift $ appendFileSafe path str

  -- | Lists files in a given path. If an error occurs, the method returns an error.
  listFilesSafe :: (E.Exception e) => Text -> m (Either e [Text])

  default listFilesSafe :: (MonadTrans t, MonadFileSystem m', m ~ t m', E.Exception e) => Text -> m (Either e [Text])
  listFilesSafe path = lift $ listFilesSafe path

instance MonadFileSystem m => MonadFileSystem (ExceptT e m)
instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (StateT s m)
instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)

instance MonadFileSystem IO where
  -- | The IO version of readFileSafe creates a new file if one doesn't exist and
  -- returns an empty string.
  readFileSafe path = do
    result <- E.try $ T.readFile (T.unpack path)
    return $ case result of
        Left err -> Left err
        Right content -> Right content
  writeFileSafe path content = do
    result <- E.try $ T.writeFile (T.unpack path) content
    return $ case result of
        Left err -> Left err
        Right _ -> Right ()
  appendFileSafe path content = do
    result <- E.try $ T.appendFile (T.unpack path) content
    return $ case result of
        Left err -> Left err
        Right _ -> Right ()
  listFilesSafe path = do
    result <- E.try $ listDirectory (T.unpack path)
    return $ case result of
        Left err -> Left err
        Right files -> Right $ map T.pack files
