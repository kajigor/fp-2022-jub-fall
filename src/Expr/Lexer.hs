module Expr.Lexer where

import Expr.Combinators
import Expr.AST
import Data.Functor (($>))
import Control.Applicative (some, many, (<|>))
import Data.Char (isSpace, isDigit, digitToInt, isAlphaNum, isLower)

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

andOp :: Parser Op
andOp = word "&&" $> And

orOp :: Parser Op
orOp = word "||" $> Or

le :: Parser Op
le = word "<=" $> Le

lt :: Parser Op
lt = word "<" $> Lt

ge :: Parser Op
ge = word ">=" $> Ge

gt :: Parser Op
gt = word ">" $> Gt

eq :: Parser Op
eq = word "==" $> Eq

neq :: Parser Op
neq = word "/=" $> Neq

trueKW :: Parser String
trueKW = word "true"

falseKW :: Parser String
falseKW = word "false"

assign :: Parser ()
assign = spaced $ char '=' $> ()

word :: String -> Parser String
word lexeme =
    spaced $ go lexeme <* (ignore (satisfy (not . isAlphaNum)) <|> eof)
  where
    go [] = pure ""
    go (h:t) = (:) <$> char h <*> go t

ifKW :: Parser String
ifKW = word "if"

thenKW :: Parser String
thenKW = word "then"

elseKW :: Parser String
elseKW = word "else"

letKW :: Parser String
letKW = word "let"

inKW :: Parser String
inKW = word "in"

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

num :: Parser Int
num = foldl (\acc x -> acc * 10 + x) 0 <$> some digit

number :: Parser Expr
number = spaced $ Number <$> num

