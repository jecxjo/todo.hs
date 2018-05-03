{-# LANGUAGE TemplateHaskell #-}

module App.Resources (
    getResource,
    listResources,
    getChangeLog,
    getLicense
  ) where

import qualified Data.ByteString as BS
import           Data.FileEmbed
import           Data.List (lookup)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)

resources :: [(FilePath, BS.ByteString)]
resources = $(embedDir "res")

getResource :: FilePath -> Maybe Text
getResource path = fmap decodeUtf8 $ lookup path resources

listResources :: [FilePath]
listResources = map fst resources

changelog :: BS.ByteString
changelog = $(embedFile "CHANGELOG.md")

getChangeLog :: Text
getChangeLog = decodeUtf8 changelog

license :: BS.ByteString
license = $(embedFile "LICENSE")

getLicense :: Text
getLicense = decodeUtf8 license
