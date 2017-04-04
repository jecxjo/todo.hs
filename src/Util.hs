{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util
    (
      subsetOf
    , removeIfExists
    , ShowIO
    , showIO
    , MonadDate(..)
    , getToday
    ) where

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Data.IORef
import Data.Time.Calendar (Day(..))

-- |subsetOf filters a list that contains a set of elements
subsetOf :: (Eq a, Foldable t) => [a] -> t a -> Bool
xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs

-- |Remove file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- |A class for showing IO a types
class ShowIO a where
  showIO :: a -> IO String

instance Show a => ShowIO a where
  showIO = return . show

instance ShowIO a => ShowIO (IORef a) where
  showIO a = readIORef a >>= showIO

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
