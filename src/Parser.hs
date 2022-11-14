module Parser where

import Expr
import qualified Infix
import qualified Prefix
import Data.Char (isSpace)

data ParserType = Prefix | Infix deriving (Show)

parse :: ParserType -> String -> Maybe Expr
parse pType str =
    case go pType str of
      Just (str, e) | null (trim str)  -> Just e
      _ -> Nothing
  where
    go Prefix = Prefix.parse
    go Infix  = Infix.parse

    trim = dropWhile isSpace