module Expr.Prefix where

import Expr.Lexer
import Expr.AST
import Expr.Combinators
import Control.Applicative

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Number
-- +1*234 -> Just ("4", ...)
parse :: String -> Maybe (String, Expr)
parse = runParser (spaces *> parsePrefix)

parsePrefix :: Parser Expr
parsePrefix =
        (BinOp <$> op <*> parsePrefix <*> parsePrefix)
    <|> number
  where
    op = anyOf [plus, minus, star, division, hat]