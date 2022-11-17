module Expr.Parser where

import Expr.AST
import qualified Expr.Infix as Infix
import qualified Expr.Prefix as Prefix
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