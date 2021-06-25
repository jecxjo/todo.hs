module Text.Color (ShowColor(..)) where

class Show a => ShowColor a where
  showColor :: a -> String
