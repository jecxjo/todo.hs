{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util
    (
      subsetOf
    , removeIfExists
    , MonadDate(..)
    , getToday
    , maybeRead
    ) where

import Control.Exception
import Data.IORef
import Data.Maybe (listToMaybe)
import Data.Time.Calendar (Day(..))
import Prelude hiding (catch)
import System.Directory
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

-- |A class for handling Dates, allowing testing to not rely on IO ()
class Monad m => MonadDate m where
  getDay :: m Day

getToday :: MonadDate m => m Day
getToday = do
  d <- getDay
  return d

-- | Filter and Modify
filterModify :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterModify _ _ [] = []
filterModify f g [x] = if (f x) then [g x] else []
filterModify f g (x:xs) = if (f x)
                          then [g x] ++ filterModify f g xs
                          else filterModify f g xs

-- |MaybeRead is like Read but instead of exception returns Nothing
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
