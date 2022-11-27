module Expr.Lexer where

import Expr.Combinators
import Expr.AST
import Data.Functor (($>))
import Control.Applicative (some, many)
import Data.Char (digitToInt, isDigit, isSpace, isLower)

spaces :: Parser ()
spaces =
    many whiteSpace $> ()
  where
    whiteSpace = satisfy isSpace

spaced :: Parser a -> Parser a
spaced p = p <* spaces

lbr :: Parser Char
lbr = spaced $ char '('

rbr :: Parser Char
rbr = spaced $ char ')'

plus :: Parser Op
plus = spaced $ char '+' $> Plus

minus :: Parser Op
minus = spaced $ char '-' $> Minus

star :: Parser Op
star = spaced $ char '*' $> Mult

division :: Parser Op
division = spaced $ char '/' $> Div

hat :: Parser Op
hat = spaced $ char '^' $> Pow

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

num :: Parser Int
num = foldl (\acc x -> acc * 10 + x) 0 <$> some digit

number :: Parser Expr
number = spaced $ Number <$> num

ident :: Parser Expr
ident = spaced $ Ident <$> some (satisfy isLower)
