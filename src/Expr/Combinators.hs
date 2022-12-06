{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Expr.Combinators where

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

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

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
    Parser f
  where
    f (c:cs) | p c = Just (cs,c)
    f _ = Nothing

char :: Char -> Parser Char
char p = satisfy (== p)

anyOf :: [Parser a] -> Parser a
anyOf = foldl (<|>) empty

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third
leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
rightAssoc f (first, rest) =
    let (beginning, last) = go (first, rest) in
    foldr (\(elem, sep) acc -> f elem sep acc) last beginning
  where
    go :: (elem, [(sep, elem)]) -> ([(elem, sep)], elem)
    go (first, []) = ([], first)
    go (first, ((sep, second) : rest)) =
      let (list, last) = go (second, rest) in
      ((first, sep) : list, last)

eof :: Parser ()
eof = Parser $ \input ->
  case input of
    [] -> Just ([], ())
    _ -> Nothing


list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep =
  (,) <$> elem <*> many ((,) <$> sep <*> elem)

notParser :: Parser a -> Parser ()
notParser p = Parser $ \input ->
  case runParser p input of
    Just _ -> Nothing
    Nothing -> pure (input, ())

ignore :: Parser a -> Parser ()
ignore p = () <$ p