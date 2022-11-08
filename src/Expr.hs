module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Exp Expr
          | Diff Expr Expr
          | Root Expr Expr  
          deriving (Show, Eq)

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | ZeroRoot
                     | RootOfNegative
                     deriving (Show, Read, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalExpEither :: Double -> Either ArithmeticError Double
totalExpEither x = Right $ exp x

totalDiffEither :: Double -> Double -> Either ArithmeticError Double
totalDiffEither a b = Right $ a - b

totalRootEither :: Double -> Double -> Either ArithmeticError Double
totalRootEither a b | b == 0 = Left ZeroRoot
                    | a < 0 = Left RootOfNegative
                    | otherwise = Right $ a ** (1 / b)

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
eval (Exp x) = do
  x' <- eval x
  totalExpEither x'
eval (Diff x y) = do
  x' <- eval x
  y' <- eval y
  totalDiffEither x' y'
eval (Root x y) = do
  x' <- eval x
  y' <- eval y
  totalRootEither x' y'

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult res = 
  case res of
    Left DivisionByZero -> [Div (Val x) (Val 0) | x <- [1..]]
    Left LogOfZero -> [Log (Diff (Val x) (Val x)) | x <- [1..]]
    Left LogOfNegativeNumber -> [Log (Val (-x)) | x <- [1..]] 
    Left ZeroRoot -> [Root (Val x) (Val 0) | x <- [1..]]
    Left RootOfNegative -> [Root (Val (-x)) (Val 2) | x <- [1..]]
    Right x -> [Diff (Diff (Val x) (Val y)) (Val (-y)) | y <- [1..]]