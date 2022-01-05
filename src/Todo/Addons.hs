{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Todo.Addons (
  listAddons,
  isAddon,
  runAddon
  ) where

import           Todo.App
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)

listAddons :: (AppConfig m, MonadFileSystem m, AppError m) => m [Text]
listAddons = do
  st <- get
  let maybePath = addonPath st
  case maybePath of
    Nothing -> throwError . EMiscError $ T.pack "Addon path not set"
    Just path -> listFiles $ T.pack path

isAddon :: (AppConfig m, MonadFileSystem m, AppError m) => Text -> m Bool
isAddon name = elem name <$> listAddons

runAddon :: (AppConfig m, MonadProcess m, MonadIO m) => String -> [(String, String)] -> String -> [String] -> m Bool
runAddon path envVar cmd args = (==0) <$> runProcess path envVar cmd args
