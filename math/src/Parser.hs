{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser where

import Data.Char
import Control.Applicative
import Data.Functor
import Expr

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
  f (c:cs) | p c = Just (cs,c)
  f _ = Nothing

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = foldr
      (\ c -> (<*>) ((:) <$> char c))
      (Parser (\ input -> Just (input, "")))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (input', res) -> Just (input', f res)
      Nothing -> Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser u <*> Parser v = Parser $ \xs ->
    case u xs of
      Nothing -> Nothing
      Just (xs', g) ->
        case v xs' of
          Nothing -> Nothing
          Just (xs'', x) -> Just (xs'', g x)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser u <|> Parser v = Parser $ \xs ->
    case u xs of
      Nothing -> v xs
      z -> z

exprParser :: Parser Expr
exprParser = -- 1-2+3  will be 1-(2+3) and I have no idea how to fix
    -- binOP <$> withoutPMParser <*> (minus <|> plus) <*> exprParser
      plusMinusParser
  <|> withoutPMParser
  where

    plusMinusParser = 
      let first = withoutPMParser in
      let term = (,) <$> (minus <|> plus) <*> withoutPMParser in
        foldl (\acc x -> BinOp (fst x) acc (snd x)) <$> first <*> some term

    withoutPMParser = -- without plus/minus
          multDivParser
      <|> withoutPMMDPareser
      <|> (UnaryOp <$> umin <*> withoutPMParser)

    multDivParser =
      let first = withoutPMMDPareser in
      let factor = (,) <$> (mult <|> divide) <*> withoutPMMDPareser  in
        foldl (\acc x -> BinOp (fst x) acc (snd x)) <$> first <*> some factor

    withoutPMMDPareser = -- without plus/minus/div/mult
      (binOP <$> (unaryParser <|> unit) <*> pow <*> (unaryParser <|> unit))
      <|> unaryParser
      <|> unit

    unaryParser = UnaryOp <$> (sin <|> cos <|> abs) <*> exprParser

    unit = numberParser <|> exprInBrkts <|> varParser

    varParser = Var <$> some (satisfy isLower)

    exprInBrkts = char '(' *> exprParser <* char ')'

    binOP l op r = BinOp op l r 

    numberParser = Number <$> number

    minus  = char '-' $> Minus
    plus   = char '+' $> Plus
    mult   = char '*' $> Mult
    divide = char '/' $> Div
    pow    = char '^' $> Pow 

    sin  = string "sin" $> Sin
    cos  = string "cos" $> Cos
    abs  = string "abs" $> Abs
    umin = char '-' $> UnaryMinus


number :: Parser Double
number = positiveDouble
         <|> (\x -> -x) <$> (char '-' *> positiveDouble)
         <|> fromIntegral <$> intNumber
  where
    positiveDouble :: Parser Double
    positiveDouble =  merge <$> (positive <* char '.') <*> some (satisfy isDigit)

    merge :: Int -> String -> Double
    merge a b = fromIntegral a + foldr addDigit 0 b

    addDigit :: Char -> Double -> Double
    addDigit c x = (x + fromIntegral (digitToInt c)) / 10

    intNumber :: Parser Int
    intNumber = (\x -> -x) <$> (char '-' *> positive) <|> positive

    positive :: Parser Int
    positive = foldl (\acc x -> acc * 10 + x) 0 <$> some digit
