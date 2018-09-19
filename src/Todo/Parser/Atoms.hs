module Todo.Parser.Atoms
    (
      whiteSpace
    ) where

import Control.Applicative (many)
import Text.Parsec.Char (char)
import Text.Parsec.Text

-- |Parse and ignore all white space
whiteSpace :: Parser ()
whiteSpace = () <$ many (char ' ')
