module Expr where

import Control.Exception (evaluate)
import Data.Either (fromRight)
import System.Random

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr
  = Val Double
  | Sum Expr Expr
  | Diff Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Log Expr
  | Exp Expr
  | Root Int Expr
  deriving (Show, Eq)

data ArithmeticError
  = DivisionByZero
  | LogOfZero
  | LogOfNegativeNumber
  | RootIsNotNatural
  | EvenRootOfNegative
  deriving (Show, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y
  | y == 0 = Left DivisionByZero
  | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x
  | x == 0 = Left LogOfZero
  | x < 0 = Left LogOfNegativeNumber
  | otherwise = Right $ log x

totalRootEither :: Int -> Double -> Either ArithmeticError Double
totalRootEither r x
  | r <= 0 = Left RootIsNotNatural
  | x < 0 && even r = Left EvenRootOfNegative
  | x < 0 && odd r = Right $ negate $ fromRight 0 (totalRootEither r (- x))
  | otherwise = Right $ x ** (1 / fromIntegral r)

eval :: Expr -> Either ArithmeticError Double
eval (Val n) = return n
eval (Div x y) = do
  x' <- eval x -- eval x >>= \x' ->
  y' <- eval y -- eval y >>= \y' ->
  totalDivEither x' y' -- totalDivEither x' y'
eval (Log x) = do
  x' <- eval x -- eval x >>= \x' ->
  totalLogEither x' -- totalLogEither x'
eval (Sum x y) = do
  x' <- eval x
  y' <- eval y
  Right $ x' + y'
eval (Diff x y) = do
  x' <- eval x
  y' <- eval y
  Right $ x' - y'
eval (Mul x y) = do
  x' <- eval x
  y' <- eval y
  Right $ x' * y'
eval (Exp x) = do
  x' <- eval x
  Right $ exp x'
eval (Root r x) = do
  x' <- eval x
  totalRootEither r x'

-- Adding last operation to get wished result
extendExprToResult :: Double -> String -> Expr -> Expr
extendExprToResult res op expr
  | op == "sum" = Sum expr (Val (res - evaluated))
  | op == "diff" = Diff expr (Val (evaluated - res))
  | op == "mul" && evaluated /= 0 = Mul expr (Val (res / evaluated))
  | op == "div" && res /= 0 = Div expr (Val (evaluated / res))
  | otherwise = undefined
  where
    evaluated = fromRight 0 (eval expr)


-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult = undefined

-- Пример выражения в нашем абстрактном синтаксисе
-- (12 / log 10) / 3
expr :: Expr
expr = Div (Div (Val 12) (Log (Val 10))) (Val 3)

-- Выражение, вычисление которого должно приводить к ошибке.
-- Содержит деление на 0 и логарифм от отрицательного числа.
-- (6 / log (-1)) / 0
exprErr :: Expr
exprErr = Div (Div (Val 6) (Log (Val (-1)))) (Val 0)

expr1 :: Expr
expr1 = Log (Val 0)

expr2 :: Expr
expr2 = Log (Val (-1))

expr3 :: Expr
expr3 = Div (Val 1) (Val 0)
