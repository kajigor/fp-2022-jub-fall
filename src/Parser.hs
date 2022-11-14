{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser where

import Data.Char
import Control.Applicative
import Data.Functor

-- Парсер -- функция, синтаксически анализирующая префикс входной строки.
-- Парсинг может закончиться ошибкой, поэтому возвращаем Maybe
-- Типовой параметр a тут -- тип результата, который наш парсер возвращает.
-- Также возвращается суффикс строки, который при синтаксическом анализе еще не был прочитан.
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- -- Успешно завершаемся, только если первая буква строки -- 'A'
-- charA :: Parser Char
-- charA = Parser $ \input ->
--   case input of
--     ('A' : t) -> Just (t, 'A')
--     _ -> Nothing

-- -- Успешно завершаемся, если первая буква строки -- цифра.
-- -- Результатом возвращаем число типа Int, полученное из первой буквы.
-- digit :: Parser Int
-- digit = Parser $ \input ->
--   case input of
--     (c : t) | isDigit c -> Just (t, digitToInt c)
--     _ -> Nothing

-- Парсер-комбинатор, проверяющий, что первая буква строки удовлетворяет предикату p.
-- Возвращаем первую букву строки как результат.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
  f (c:cs) | p c = Just (cs,c)
  f _ = Nothing

-- Реализация парсера одной цифры через satisfy.
-- satisfy isDigit возвращает как результат символ типа Char,
-- поэтому мы дополнительно применяем к результату функцию digitToInt, используя fmap (<$>).
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- Парсер буквы 'A'
charA :: Parser Char
charA = satisfy (== 'A')

-- Парсер, принимающий любую строчную букву.
lower :: Parser Char
lower = satisfy isLower

-- Парсер, проверяющий, что первым символом строки является символ c
char :: Char -> Parser Char
char c = satisfy (== c)

-- Парсер является функтором.
instance Functor Parser where
  -- Запускаем парсер p, к результату применяем функцию f
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (input', res) -> Just (input', f res)
      Nothing -> Nothing

-- Парсер является аппликативом
instance Applicative Parser where
  -- Парсер, полученный из pure x, всегда завершается успехом,
  -- не читает ничего из строки, возвращает x как результат.
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)

  -- Запускаем первый парсер на входе.
  -- Если он завершился успешно, его результат -- функция g :: a -> b.
  -- Теперь запускаем второй парсер на суффиксе строки.
  -- Если второй парсер завершился успешно, его результат имеет тип a: x :: a.
  -- Комбинируем результаты, применяя функцию g к x, получая результат типа b.
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser u <*> Parser v = Parser $ \xs ->
    case u xs of
      Nothing -> Nothing
      Just (xs', g) ->
        case v xs' of
          Nothing -> Nothing
          Just (xs'', x) -> Just (xs'', g x)

-- Парсер, который проверяет, что префикс строки имеет вид n*m, где n и m -- цифры.
-- Если это так, то применяет функцию умножения к n и m, возвращая это как результат.
-- Оператор (<*) :: f a -> f b -> f a работает, как (<*>), но игнорирует результат своего правого аргумента.
multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit

-- (<$>) = fmap
-- f <$> g = pure f <*> g

-- Тип данных для представления арифметических выражений
-- "1+2*3" -> BinOp Plus (Number 1) (BinOp Mult (NUmber 2) (NUbmer 3))
-- "1+*3" -> Fail
data Expr = BinOp Op Expr Expr
          | Number Int
          deriving (Show, Eq)

data Op = Plus
        | Minus
        | Mult
        | Div
        deriving (Show, Eq)

-- Парсер, похожий на multiplication, только он строит результатом значение типа данных Expr
mult :: Parser Expr
mult = (\x y -> BinOp Mult (Number x) (Number y)) <$> digit <* char '*' <*> digit

-- Если существует 2 возможных варианта что-то распарсить, используем (<|>) из класса Alternative
instance Alternative Parser where
  -- Парсер, который всегда завершается ошибкой.
  -- Единица бинарного отношения (<|>)
  empty :: Parser a
  empty = Parser $ const Nothing

  -- Первый или второй парсер.
  -- Запускаем первый парсер.
  -- Если он завершился успешно -- возвращаем это как результат, второй парсер не запускается.
  -- Если первый парсер сообщил об ошибке, и только в этом случае, -- запускаем второй парсер.
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser u <|> Parser v = Parser $ \xs ->
    case u xs of
      Nothing -> v xs
      z -> z

-- Парсер для арифметических выражений с операциями +/- и *// с правильными приоритетами
-- 1 + 1 + 2 * 3 = (1 + 1) + (2 * 3)
-- (((1) + 2) * (3)) = (1 + 2) * 3
exprParser :: Parser Expr
exprParser =                                                 -- выражение это
    mkAST <$> multParser <*> (minus <|> plus) <*> exprParser -- либо сумма/разность множителей
          <|> multParser                                     -- либо один множитель
  where
    numberParser = Number <$> number -- число
    multParser =                                            -- множитель это
      mkAST <$> factor <*> (mult <|> divide) <*> multParser -- либо произведение/деление факторов
            <|> factor                                      -- либо один фактор

    factor = numberParser <|> exprInBrkts -- фактор это либо число, либо выражение в круглых скобках

    exprInBrkts = -- игнорируем результат, возвращаемый парсерами скобок
      char '(' *> exprParser <* char ')'

    mkAST l op r = BinOp op l r -- создаем значение нашего типа данных Expr
    minus  = char '-' $> Minus -- Игнорируем результат, возвращаемый char, сразу создаем значение типа Op
    plus   = char '+' $> Plus
    mult   = char '*' $> Mult
    divide = char '/' $> Div

-- Число -- несколько (не меньше одной) цифр, которые мы дальше преобразовываем в число типа Int
-- some :: Alternative f => f a -> f [a] -- стандартная функция для одного или большего количества повторений p
-- По смыслу some p = ((:) <$> p <*> some p) <|> ((:[]) <$> p)
number :: Parser Int
number = foldl (\acc x -> acc * 10 + x) 0 <$> some digit

-- Интерпретатор выражений (калькулятор)
eval :: Expr -> Int
eval (Number x) = x
eval (BinOp op x y) =
    fromOp op (eval x) (eval y)
  where
    fromOp Plus = (+)
    fromOp Minus = \x y -> x - y
    fromOp Mult = (*)
    fromOp Div = div