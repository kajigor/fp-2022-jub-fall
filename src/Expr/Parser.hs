module Expr.Parser where

import Expr.AST
import qualified Expr.Infix as Infix
import Data.Char (isSpace)

parse :: String -> Maybe Expr
parse str =
    case Infix.parse str of
      Just (str, e) | null (trim str) -> Just e
      _ -> Nothing
  where
    trim = dropWhile isSpace