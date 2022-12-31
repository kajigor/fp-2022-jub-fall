{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Parser (parse) where

import Lexer
import Expr
import Combinators
import Control.Applicative ((<|>))
import Data.Char (isSpace)

parseExpr :: Parser Expr
parseExpr = parseSum

parseSum :: Parser Expr
parseSum = leftAssoc toBinOp <$> list parseMult (plus <|> minus)

parseMult :: Parser Expr
parseMult = leftAssoc toBinOp <$> list (number <|> exprBr <|> x) (star <|> division)

toBinOp :: Expr -> Op -> Expr -> Expr
toBinOp l op = BinOp op l

exprBr :: Parser Expr
exprBr = lbr *> parseSum <* rbr

parse :: String -> Maybe Expr
parse str =
    case runParser parseExpr (trim str) of
      Just (str, e) | null (trim str)  -> Just e
      _ -> Nothing
  where
    trim = dropWhile isSpace