{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Parser where

import Lambda
import Data.Char
import Control.Applicative
import Data.Functor

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
  f (c:cs) | p c = Just (cs,c)
  f _ = Nothing

isLetter1 :: Char -> Bool
isLetter1 c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

letter :: Parser Char
letter = satisfy isLetter1

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser Char
space = satisfy (== ' ')

spaces :: Parser String
spaces = many space

letterString :: Parser String
letterString = foldl (\acc x -> acc ++ [x]) "" <$> some letter

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

exprParser :: Parser (Lambda String)
exprParser =                                                 
    getApp <|> absParser <|> varParser <|> exprInBrkts
    where
      appParser = ((\x -> \y -> x : y) <$> ((varParser <|> exprInBrkts)) <*> appParser)
              <|> ((\x -> [x]) <$> (varParser <|> exprInBrkts))
      getApp = (\lst -> foldl (App) (head lst) (tail lst)) <$> appParser
      varParser = Var <$> (spaces *> letterString <* spaces) 
      absParser = Abs <$> (spaces *> (char 'λ') *> spaces *> letterString <* spaces <* (char '.')) <*> exprParser
      exprInBrkts =
        spaces *> (char '(' *> exprParser <* char ')') <* spaces
    
