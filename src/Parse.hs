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

listToApp :: [Lambda] -> Lambda
listToApp [] = undefined
listToApp [f] = f
listToApp (f : x : xs) = listToApp (App f x : xs)

parseNoApp :: Parser Lambda
parseNoApp = try parseAbs <|> try parseVar <|> try parseBrackets

parseApp :: Parser Lambda
parseApp = do
  f <- try parseVar <|> try parseBrackets
  _space <- char ' '
  xs <- parseNoApp `sepBy` char ' '
  return $ listToApp (f : xs)

parseLambda :: Parser Lambda
parseLambda = space *> (try parseAbs <|> try parseApp <|> try parseVar <|> try parseBrackets) <* space