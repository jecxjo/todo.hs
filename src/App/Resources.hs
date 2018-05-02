{-# LANGUAGE TemplateHaskell #-}

module App.Resources ( getResource, listResources ) where

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
