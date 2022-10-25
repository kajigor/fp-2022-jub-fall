{-# LANGUAGE NumDecimals #-}
module Expr where

import GHC.Natural (Natural)

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Exp Expr
          | Root Expr Natural
          deriving (Show, Eq)

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | EvenRootOfNegativeNumber
                     deriving (Show, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalRootEither :: Double -> Natural -> Either ArithmeticError Double
totalRootEither x n | x < 0 && even n = Left EvenRootOfNegativeNumber
                    | otherwise = Right $ x ** (1 / fromIntegral n)

-- В Haskell есть do-нотация, которая позволяет выражать то же самое чуть более приятно:
eval :: Expr -> Either ArithmeticError Double
eval (Val n) = return n
eval (Div x y) = do
  x' <- eval x         -- eval x >>= \x' ->
  y' <- eval y         -- eval y >>= \y' ->
  totalDivEither x' y' -- totalDivEither x' y'
eval (Log x) = do
  x' <- eval x         -- eval x >>= \x' ->
  totalLogEither x'    -- totalLogEither x'
eval (Add x y) = do
  x' <- eval x         -- eval x >>= \x' ->
  y' <- eval y
  Right $ x' + y'
eval (Sub x y) = do
  x' <- eval x         -- eval x >>= \x' ->
  y' <- eval y
  Right $ x' - y'
eval (Mul x y) = do
  x' <- eval x         -- eval x >>= \x' ->
  y' <- eval y
  Right $ x' * y'
eval (Exp x) =
  do
  x' <- eval x
  Right $ exp x'
eval (Root x n) = do
  x' <- eval x         -- eval x >>= \x' ->         -- eval y >>= \y' ->
  totalRootEither x' n -- totalDivEither x' y'



-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult result = generateExprByResult' 1 where
  generateExprByResult' cnt = generateExpr result cnt : generateExprByResult' (cnt + 1)

generateExpr :: Either ArithmeticError Double -> Int -> Expr
generateExpr (Left err) depth = Add (justGen depth 1) (generateError err)
generateExpr (Right result) depth = generateExpr' (justGen depth 1) where
  generateExpr' expr = Add expr (Val (result - getResult (eval expr)))

getResult :: Either ArithmeticError Double -> Double
getResult (Right result) = result
getResult _ = 42


justGen :: Int -> Int -> Expr
justGen 1 f = Val (fromIntegral (f + 1) * 2.1)
justGen 2 1 = Log (justGen 1 0)
justGen 2 2 = Exp (justGen 1 0)
justGen depth func = funcGen func (justGen (depth `div` 2) (newFunc func)) (justGen ((depth + 1) `div` 2) (newFunc (func + 1)))

funcGen :: (Eq a, Num a) => a -> Expr -> Expr -> Expr
funcGen 0 = Div
funcGen 1 = Mul
funcGen 2 = Add
funcGen _ = Add

newFunc :: Int -> Int
newFunc f = (f + 2) `mod` 3

generateError :: ArithmeticError -> Expr
generateError DivisionByZero = Div (Val 42) (Val 0)
generateError LogOfZero = Log (Val 0)
generateError LogOfNegativeNumber = Log (Val (-42))
generateError EvenRootOfNegativeNumber = Root (Val (-42)) 2