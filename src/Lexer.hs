module Lexer where

import Combinators
import Expr
import Data.Functor (($>))
import Control.Applicative (some, (<|>))
import Data.Char (isDigit)

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

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser String
integer = some digit

nonInteger :: Parser String
nonInteger = (++) <$> integer <*> ((:) <$> char '.' <*> integer)

number :: Parser Expr
number = Number . read <$> (nonInteger <|> integer)