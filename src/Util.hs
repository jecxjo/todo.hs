module Util
    (
      subsetOf
    , removeIfExists
    ) where

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

-- |subsetOf filters a list that contains a set of elements
subsetOf :: (Eq a, Foldable t) => [a] -> t a -> Bool
xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs

-- |Remove file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
