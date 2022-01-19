{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.FileSystem (
    MonadFileSystem(..)
  ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer (WriterT)
import           Data.Bool (bool)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Prelude hiding (readFile, writeFile, appendFile)
import           System.Directory (doesFileExist, listDirectory)

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns it contents. If an error
  -- occurs, the method returns an error.
  readFile :: Text -> m Text

  default readFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m Text
  readFile = lift . readFile

  -- | Writes a file at the given path. If an error occurs, the method returns
  -- an error.
  writeFile :: Text -> Text -> m ()

  default writeFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> Text -> m ()
  writeFile path str = lift $ writeFile path str

  -- | Append a file at the given path. If an error occurs, the method returns
  -- an error.
  appendFile :: Text -> Text -> m ()

  default appendFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> Text -> m ()
  appendFile path str = lift $ appendFile path str

  -- | Lists files in a given path. If an error occurs, the method returns an error.
  listFiles :: Text -> m [Text]

  default listFiles :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m [Text]
  listFiles path = lift $ listFiles path

instance MonadFileSystem m => MonadFileSystem (ExceptT e m)
instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (StateT s m)
instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)

instance MonadFileSystem IO where
  -- | The IO version of readFile creates a new file if one doesn't exist and
  -- returns an empty string.
  readFile path = doesFileExist (T.unpack path)
                >>= bool (T.writeFile (T.unpack path) T.empty >> return T.empty)
                         (T.readFile $ T.unpack path)
  writeFile path = T.writeFile (T.unpack path)
  appendFile path = T.appendFile (T.unpack path)
  listFiles path = fmap (map T.pack) $ listDirectory $ T.unpack path

