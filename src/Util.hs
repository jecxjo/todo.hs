module Util ( subsetOf ) where

-- |subsetOf filters a list that contains a set of elements
subsetOf :: (Eq a, Foldable t) => [a] -> t a -> Bool
xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs
