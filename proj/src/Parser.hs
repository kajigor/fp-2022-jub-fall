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
      appParser = ((\x -> \y -> x : y) <$> ( (varParser <|> exprInBrkts) <* (char ' ')) <*> appParser)
              <|> ((\x -> [x]) <$> (varParser <|> exprInBrkts))
      getApp = (\lst -> foldl (App) (head lst) (tail lst)) <$> appParser
      varParser = Var <$> letterString
      absParser = Abs <$> ((char 'λ') *> letterString <* (char '.')) <*> exprParser
      exprInBrkts =
        char '(' *> exprParser <* char ')'
    




-- checkString :: String -> Parser String
-- checkString x = Parser f where
--     f xs | (helpfunction x xs) = Just ((getTail x xs), x)
--         where 
--             helpfunction x xs | (x == []) = True
--                               | (xs == []) = False
--                               | (head x) /= (head xs) = False
--                               | otherwise = helpfunction (tail x) (tail xs)
--             getTail x xs | (x == []) = xs
--                          | otherwise = getTail (tail x) (tail xs)
--     f _ = Nothing

-- exprParser :: Parser (Lambda String)
-- exprParser =                                                 
--     appParser <|> absParser <|> varParser <|> exprInBrkts
--     where
--       appParser = App <$> ((checkString "App") *> (char ' ') *> (exprParser <* (char ' '))) <*> exprParser 
--       varParser = Var <$> ((checkString "Var ") *> letterString)
--       absParser = Abs <$> ((checkString "Abs") *> (char ' ') *> letterString) <*> ((char ' ') *> exprParser)
--       exprInBrkts =
--         char '(' *> exprParser <* char ')'


-- exprParser :: Parser Expr
-- exprParser =                                                 -- выражение это
--     mkAST <$> multParser <*> (minus <|> plus) <*> exprParser -- либо сумма/разность множителей
--           <|> multParser                                     -- либо один множитель
--   where
--     variableParser = (\x -> Var x) <$> letterString         -- число
--     multParser =                                            -- множитель это
--       mkAST <$> factor <*> (mult <|> divide) <*> multParser -- либо произведение/деление факторов
--             <|> factor                                      -- либо один фактор

--     factor = numberParser <|> exprInBrkts -- фактор это либо число, либо выражение в круглых скобках

--     exprInBrkts = -- игнорируем результат, возвращаемый парсерами скобок
--       char '(' *> exprParser <* char ')'

--     mkAST l op r = BinOp op l r -- создаем значение нашего типа данных Expr
--     minus  = char '-' $> Minus -- Игнорируем результат, возвращаемый char, сразу создаем значение типа Op
--     plus   = char '+' $> Plus
--     mult   = char '*' $> Mult
--     divide = char '/' $> Div