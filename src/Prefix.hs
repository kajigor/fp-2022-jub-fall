module Prefix where

import Lexer
import Expr
import Combinators
import Control.Applicative

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Digit -- Только одинокие цифры; пробелы не разрешены
-- +1*234 -> Just ("4", ...)
parse :: String -> Maybe (String, Expr)
parse = runParser parsePrefix

parsePrefix :: Parser Expr
parsePrefix =
        (BinOp <$> op <*> parsePrefix <*> parsePrefix)
    <|> Number <$> digit
  where
    op = anyOf [plus, minus, star, division, hat]