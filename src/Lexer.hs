module Lexer where

import Combinators
import Expr
import Data.Functor (($>))
import Control.Applicative (some)
import Data.Char (digitToInt, isDigit)
import GHC.Float (int2Double)

lbr :: Parser Char
lbr = char '('

rbr :: Parser Char
rbr = char ')'

plus :: Parser Op
plus = char '+' $> Plus

minus :: Parser Op
minus = char '-' $> Minus

star :: Parser Op
star = char '*' $> Mult

division :: Parser Op
division = char '/' $> Div

x :: Parser Expr
x = X <$ char 'x'

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

number :: Parser Expr
number = Number . foldl (\acc x -> acc * 10 + int2Double x) 0.0 <$> some digit