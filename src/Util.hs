{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util
    (
      subsetOf
    , removeIfExists
    , MonadDate(..)
    , getToday
    , maybeRead
    , readChar
    ) where

import Control.Exception
import Data.Maybe (listToMaybe)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day(..))
import System.Directory
import System.IO.Error
import System.IO (hSetBuffering, hGetBuffering, stdin, BufferMode(..))

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

instance MonadDate IO where
  getDay = do
    c <- getCurrentTime
    return $ utctDay c

-- |MaybeRead is like Read but instead of exception returns Nothing
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- |Read a single character on input
readChar :: IO Char
readChar = do
  cBuffering <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin cBuffering
  return c
