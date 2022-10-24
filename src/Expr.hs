module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr
  = Val Double
  | Div Expr Expr
  | Log Expr
  | Sum Expr Expr
  | Diff Expr Expr
  | Mult Expr Expr
  | Exp Expr
  | Root Expr Expr -- Root 2 x == sqrt(x)
  deriving (Show, Eq)

data ArithmeticError
  = DivisionByZero
  | LogOfZero
  | LogOfNegativeNumber
  | NegativeRadicand
  | ZeroDegreeRoot
  deriving (Show, Eq)

totalSum :: Double -> Double -> Either ArithmeticError Double
totalSum x y = Right $ x + y

totalDiff :: Double -> Double -> Either ArithmeticError Double
totalDiff x y = Right $ x - y

totalMult :: Double -> Double -> Either ArithmeticError Double
totalMult x y = Right $ x * y

totalExp :: Double -> Either ArithmeticError Double
totalExp x = Right $ exp x

totalRoot :: Double -> Double -> Either ArithmeticError Double
totalRoot degree radicand
  | degree == 0 = Left ZeroDegreeRoot
  | radicand < 0 = Left NegativeRadicand
  | otherwise = Right $ radicand ** (1 / degree)

totalDiv :: Double -> Double -> Either ArithmeticError Double
totalDiv x y
  | y == 0 = Left DivisionByZero
  | otherwise = Right $ x / y

totalLog :: Double -> Either ArithmeticError Double
totalLog x
  | x == 0 = Left LogOfZero
  | x < 0 = Left LogOfNegativeNumber
  | otherwise = Right $ log x

-- В Haskell есть do-нотация, которая позволяет выражать то же самое чуть более приятно:
eval :: Expr -> Either ArithmeticError Double
eval (Val n) = return n
eval (Div x y) = do
  x' <- eval x -- eval x >>= \x' ->
  y' <- eval y -- eval y >>= \y' ->
  totalDiv x' y' -- totalDiv x' y'
eval (Log x) = do
  x' <- eval x -- eval x >>= \x' ->
  totalLog x' -- totalLog x'
eval (Sum x y) = do
  x' <- eval x
  y' <- eval y
  totalSum x' y'
eval (Diff x y) = do
  x' <- eval x
  y' <- eval y
  totalDiff x' y'
eval (Mult x y) = do
  x' <- eval x
  y' <- eval y
  totalMult x' y'
eval (Exp x) = do
  x' <- eval x
  totalExp x'
eval (Root degree radicand) = do
  degree' <- eval degree
  radicand' <- eval radicand
  totalRoot degree' radicand'

expr1 :: Expr
expr1 = Log (Val 0)

expr2 :: Expr
expr2 = Log (Val (-1))

expr3 :: Expr
expr3 = Div (Val 1) (Val 0)

expr4 :: Expr
expr4 = Sum (Val 1) (Root (Val 2) (Val 4))

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult = undefined
