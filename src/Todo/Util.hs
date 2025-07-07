{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo.Util
    (
      subsetOf
    , removeIfExists
    , maybeRead
    , readChar
    , digitCount
    , showPaddedNumber
    , notEmpty
    , maybeFilter
    , maybeToEither
    , replace
    ) where

import           Control.Exception
import           Data.Bool (bool)
import           Data.Maybe (listToMaybe)
import           System.Directory
import           System.IO.Error
import           System.IO (hSetBuffering, hGetBuffering, stdin, BufferMode(..))

-- |subsetOf filters a list that contains a set of elements
subsetOf :: (Eq a, Foldable t) => [a] -> t a -> Bool
xs `subsetOf` ys = not (any (not . (`elem` ys)) xs)

-- |Remove file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

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

-- |Calculate the number of digits
digitCount :: Int -> Integer
digitCount x
    | x < 0 = digitCount $ abs x
    | x < 10 = 1
    | otherwise = 1 + digitCount ((x - (x `mod` 10)) `div` 10)

-- |Show Number with padding
showPaddedNumber :: Char -> Int -> Int -> String
showPaddedNumber c width number =
  replicate (width - fromIntegral (digitCount number)) c ++ show number

-- | Similar to maybe and bool, the notEmpty applies a function on a non-empty list
notEmpty :: b -> ([a] -> b) -> [a] -> b
notEmpty emptyRes nonEmptyFn lst = bool emptyRes (nonEmptyFn lst) (0 < length lst)

-- | Similar to filter, returning Nothing if nothing was filtered out, otherwise Just [a]
maybeFilter :: (a -> Bool) -> [a] -> Maybe [a]
maybeFilter fn lst = do
  let filtered = filter fn lst
  bool Nothing (Just filtered) (not (null filtered))

-- | Converts Maybe to Either
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

-- | Replace elements from a list
replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c
