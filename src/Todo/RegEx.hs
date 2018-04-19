module Todo.RegEx
  ( matchGen
  , swapGen
  , swapAllGen
  ) where

import Data.Text (pack, unpack, replace, Text(..))
import Text.Regex.PCRE

-- |matchGen generates a search function based on a regular expression
matchGen :: String -> (String -> Bool)
matchGen "" = const True
matchGen re = \input -> input =~ re

-- |swapGen: re -> replace -> (oldStr -> newStr)
-- oldstr -> s/re/replace/ -> newStr
swapGen :: String -> String -> (String -> String)
swapGen "" _ = id
swapGen re sw = \input ->
  case input =~ re of
    (b, "", "", []) -> b -- No swap
    (b, _, a, [])   -> b ++ sw ++ a -- Plain swap
    (b, _, a, subs) -> b ++ (doSubs (zip [1..] subs) sw) ++ a
      where
        doSubs subs text = foldl (\str (num, rep) ->
                                    unpack $ replace (pack $ "\\" ++ show num)
                                                    (pack rep)
                                                    (pack str))
                                 text subs

swapAllGen :: String -> String -> (String -> String)
swapAllGen "" _ = id
swapAllGen re sw = swapFn
  where swapFn = \input ->
          case input =~ re of
            (b, "", "", []) -> b -- No swap
            (b, _, a, [])   -> b ++ sw ++ (swapFn a) -- Plain swap
            (b, _, a, subs) -> b ++ (doSubs (zip [1..] subs) sw) ++ (swapFn a)
              where
                doSubs subs text = foldl (\str (num, rep) ->
                                            unpack $ replace (pack $ "\\" ++ show num)
                                                            (pack rep)
                                                            (pack str))
                                        text subs
