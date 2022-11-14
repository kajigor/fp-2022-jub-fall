module Lexer where

import Combinators
import Expr
import Data.Functor (($>))
import Control.Applicative (some)
import Data.Char (digitToInt, isDigit)

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

hat :: Parser Op
hat = char '^' $> Pow

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

number :: Parser Expr
number = Number <$> foldl (\acc x -> acc * 10 + x) 0 <$> some digit