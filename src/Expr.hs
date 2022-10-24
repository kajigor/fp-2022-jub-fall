module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Sum Expr Expr
          | Dif Expr Expr
          | Mul Expr Expr
          | Exp Expr
          | Root Expr Int
          deriving (Show, Eq)

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | RootOfZeroDegree
                     | EvenRootOfNegativeNumber
                     deriving (Show, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalRootEiter :: Double -> Int -> Either ArithmeticError Double
totalRootEiter x k | k == 0 = Left RootOfZeroDegree
                   | x < 0 && even k = Left EvenRootOfNegativeNumber
                   | otherwise = Right $ x * (1.0 / fromIntegral k)


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
eval (Sum x y) = do
  x' <- eval x
  y' <- eval y
  Right $ x' + y'
eval (Dif x y) = do
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
eval (Root x k) = do
  x' <- eval x
  totalRootEiter x' k


-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult value = case value of
  Left DivisionByZero -> [Div (Val a) (Val 0) | a <- [1..]]
  Left LogOfZero -> [Log x | x <- generateExprByResult $ Right 0]
  Left RootOfZeroDegree -> [Root (Val x) 0 | x <- [1..]]
  Left LogOfNegativeNumber -> [Log (Val x) | x <-[-1, -2..]]
  Left EvenRootOfNegativeNumber -> [Root (Val x) k | x <- [-1, -2..], k <- [2, 4..]]
  Right x -> [Sum (Val a) (Val b) | a <- [1..], b <- [x - a]]

