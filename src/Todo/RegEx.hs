module Todo.RegEx
  ( matchGen
  , swapGen
  , swapAllGen
  ) where

import Data.Text (pack, unpack, replace)
import Text.Regex.PCRE

doSubs :: [(Int, String)] -> String -> String
doSubs subs text = foldl replacePlaceholder text subs
  where
    replacePlaceholder str (num, rep) =
      unpack $ replace (pack $ "\\" ++ show num) (pack rep) (pack str)

-- |matchGen generates a search function based on a regular expression
matchGen :: String -> (String -> Bool)
matchGen "" = const True
matchGen re = (=~ re)

-- |swapGen: re -> replace -> (oldStr -> newStr)
-- oldstr -> s/re/replace/ -> newStr
swapGen :: String -> String -> (String -> String)
swapGen "" _ = id
swapGen re sw = \input ->
  case input =~ re of
    (b, "", "", []) -> b -- No swap
    (b, _, a, s) -> b ++ doSubs (zip [1..] s) sw ++ a

swapAllGen :: String -> String -> (String -> String)
swapAllGen "" _ = id
swapAllGen re sw = swapFn
  where swapFn input =
          case input =~ re of
            (b, "", "", []) -> b -- No swap
            (b, _, a, s) -> b ++ doSubs (zip [1..] s) sw ++ swapFn a
