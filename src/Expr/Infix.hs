{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Expr.Infix where

import Expr.Lexer
import Expr.AST
import Expr.Combinators
import Control.Applicative ((<|>))
import Data.Functor (($>))

parse :: String -> Maybe (String, Expr)
parse = runParser (spaces *> parseExpr)

parseExpr :: Parser Expr
parseExpr = parseOr

parseOr :: Parser Expr
parseOr = rightAssoc toBinOp <$> list parseAnd andOp

parseAnd :: Parser Expr
parseAnd = rightAssoc toBinOp <$> list parseCmp orOp

parseCmp :: Parser Expr
parseCmp = toBinOp <$> parseSum <*> (le <|> lt <|> ge <|> gt <|> eq <|> neq) <*> parseSum
           <|> parseSum

parseIf :: Parser Expr
parseIf =
  If <$> (ifKW *> parseExpr) <*> (thenKW *> parseExpr) <*> (elseKW *> parseExpr)

parseSum :: Parser Expr
parseSum = leftAssoc toBinOp <$> list parseMult (plus <|> minus)

parseMult :: Parser Expr
parseMult = leftAssoc toBinOp <$> list parsePow (star <|> division)

parsePow :: Parser Expr
parsePow = rightAssoc toBinOp <$> list (number <|> exprBr <|> bool <|> parseIf) hat

bool :: Parser Expr
bool = Boolean <$> (trueKW $> True <|> falseKW $> False)

toBinOp :: Expr -> Op -> Expr -> Expr
toBinOp l op r = BinOp op l r

exprBr :: Parser Expr
exprBr = lbr *> parseExpr <* rbr
