{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parse where

import Data.Char
import Data.Void
import Lambda
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List.NonEmpty

type Parser = Parsec Void String

parsePretty :: Show a => Parser a -> String -> String
parsePretty p input =
  case parse p "" input of
    Left e -> errorBundlePretty e
    Right x -> show x

parseVar :: Parser Lambda
parseVar = do
  name <- some $ satisfy isAlphaNum
  return $ Var name

parseAbs :: Parser Lambda
parseAbs = do
  _lambda <- char '\\' <|> char 'λ'
  name <- some $ satisfy isAlphaNum
  _dot <- char '.'
  body <- parseLambda
  return $ Abs name body

parseBrackets :: Parser Lambda
parseBrackets = do
  _open <- char '('
  result <- parseLambda
  _close <- char ')'
  return result

listToApp :: NonEmpty Lambda -> Lambda
listToApp (f :| xs) = foldl App f xs

parseNoApp :: Parser Lambda
parseNoApp = parseAbs <|> parseVar <|> parseBrackets

parseApp :: Parser Lambda
parseApp = do
  f <- parseVar <|> parseBrackets
  _space <- char ' '
  xs <- parseNoApp `sepBy` char ' '
  return $ listToApp (f :| xs)

parseLambda :: Parser Lambda
parseLambda = space *> (try parseApp <|> parseNoApp) <* space