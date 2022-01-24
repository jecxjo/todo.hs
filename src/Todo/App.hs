{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Todo.App (
    ErrorType(..),
    Options(..),
    AppConfig,
    AppError,
    AppM(..),
    runApp,
    emptyOptions,
    initOptions,
    renderError,
    module Control.Monad.State,
    module Control.Monad.Except,
    module Control.Monad.Arguments,
    module Control.Monad.Date,
    module Control.Monad.EnvVar,
    module Control.Monad.FileSystem,
    module Control.Monad.Process,
    module Control.Monad.IO.Class
  ) where

import qualified Control.Exception as E
import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Arguments (MonadArguments(..))
import           Control.Monad.EnvVar (MonadEnvVar(..))
import           Control.Monad.FileSystem (MonadFileSystem(..))
import           Control.Monad.Process (MonadProcess(..))
import           Control.Monad.Date (MonadDate(..))
import           Control.Monad.State (StateT, MonadState, runStateT, get, put, modify)
import           Data.List (intercalate)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Time.Calendar (Day(..))
import           Text.Parsec.Error as PE

data Options = Options {
                         todoTxtPath :: FilePath
                       , archiveTxtPath :: Maybe FilePath
                       , reportTxtPath :: Maybe FilePath
                       , timeStamp :: Maybe Day
                       , autoAccept :: Maybe Bool
                       , forcedPrompt :: Bool
                       , prettyPrinting :: Bool
                       , addonPath :: Maybe FilePath
                       }
                       deriving (Show, Eq)

type AppConfig = MonadState Options

emptyOptions :: Options
emptyOptions = Options "" Nothing Nothing Nothing Nothing False False Nothing

initOptions :: FilePath -> Bool -> Options
initOptions path prettyPrinting = Options path Nothing Nothing Nothing Nothing False prettyPrinting Nothing

data ErrorType
  = EInvalidIndex Int
  | EInvalidIndexes [Int]
  | EInvalidArg Text
  | EIOError E.IOException
  | EMiscError Text
  | EParseError PE.ParseError
  | EShortCircuit Text
  deriving (Show, Eq)

type AppError = MonadError ErrorType

newtype AppM a = AppM {
  runAppM :: StateT Options (ExceptT ErrorType IO) a
} deriving (Monad, Functor, Applicative,
            AppConfig, MonadIO, MonadError ErrorType,
            MonadArguments, MonadEnvVar, MonadFileSystem,
            MonadProcess, MonadDate)

runApp :: Options -> AppM () -> IO ()
runApp o app = either (T.putStrLn . renderError) (return . fst) =<< runExceptT (runStateT (runAppM app) o)

renderError :: ErrorType -> Text
renderError (EInvalidIndex idx) = "todo: invalid index -- " <> T.pack (show idx)
renderError (EInvalidIndexes idx) = "todo: invalid index -- " <> T.intercalate ", " (map (T.pack . show) idx)
renderError (EInvalidArg arg) = "todo: invalid argument -- '" <> arg <> "'"
renderError (EIOError e) = "todo: " <> T.pack (E.displayException e)
renderError (EMiscError t) = "todo: " <> t
renderError (EParseError pe) = "todo: parse error(s) -- " <> T.pack (show $ PE.errorPos pe) <> " " <> T.intercalate "\n" (map (T.pack . PE.messageString) $ PE.errorMessages pe)
renderError (EShortCircuit txt) = txt
