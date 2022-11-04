module Expr where

data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Sum Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Exp Expr
          | Root Int Expr
          deriving (Show, Eq)

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | RootNegativeNumber
                     | RootZeroDegree
                     deriving (Show, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalRootEither :: Int -> Double -> Either ArithmeticError Double
totalRootEither d x | d == 0 = Left RootZeroDegree
                    | x < 0 = Left RootNegativeNumber 
                    | otherwise = Right $ x **  (1.0 / fromIntegral d)

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
eval (Sub x y) = do
   x' <- eval x
   y' <- eval y
   Right $ x' - y'
eval (Mult x y) = do
   x' <- eval x
   y' <- eval y
   Right $ x' * y'
eval (Exp x) = do
  x' <- eval x
  Right $ exp x'
eval (Root d x) = do
  x' <- eval x
  totalRootEither d x'

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult value = 
  case value of 
    Left DivisionByZero -> [Div (Val x) (Val 0) | x <- [1..]]
    Left LogOfZero -> [Sum (Val x) (Log (Val 0)) | x <- [1..]]
    Left LogOfNegativeNumber -> [Log (Val (x)) | x <- [-1, -2..]]
    Left RootNegativeNumber -> [Root x (Val (-1)) | x <- [2, 4..]]
    Left RootZeroDegree -> [Root 0 (Val x) | x <- [1..]]
    Right v -> [Sum (Val x) (Val 0) | x <- [1..]]
