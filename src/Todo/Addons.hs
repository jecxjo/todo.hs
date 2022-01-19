{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Todo.Addons (
  listAddons
  ) where

import           Todo.App
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)

listAddons :: (AppConfig m, MonadFileSystem m) => m [Text]
-- listAddons = (addonPath <$> get) >>= return . maybe [] (\path -> liftIO . listFiles $ T.pack path)
listAddons = do
  st <- get
  let maybePath = addonPath st
  case maybePath of
    Nothing -> return []
    Just path -> listFiles $ T.pack path

