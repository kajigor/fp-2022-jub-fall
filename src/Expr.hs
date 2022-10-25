module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data BinOp = Add | Sub | Mul | Div | Root deriving (Show, Eq)

data UnOp = Log | Exp deriving (Show, Eq)

data Expr
  = Val Double
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  deriving (Show, Eq)

data ArithmeticError
  = DivisionByZero
  | RootOfDegreeZero
  | RootOfNegativeNumber
  | LogOfNonPositiveNumber
  deriving (Show, Eq)

addEither :: Double -> Double -> Either ArithmeticError Double
addEither x y = Right $ x + y

subEither :: Double -> Double -> Either ArithmeticError Double
subEither x y = Right $ x - y

mulEither :: Double -> Double -> Either ArithmeticError Double
mulEither x y = Right $ x * y

divEither :: Double -> Double -> Either ArithmeticError Double
divEither x y
  | y == 0 = Left DivisionByZero
  | otherwise = Right $ x / y

rootEither :: Double -> Double -> Either ArithmeticError Double
rootEither d val
  | d == 0 = Left RootOfDegreeZero
  | val < 0 = Left RootOfNegativeNumber
  | otherwise = Right $ exp $ d * log val

logEither :: Double -> Either ArithmeticError Double
logEither x
  | x <= 0 = Left LogOfNonPositiveNumber
  | otherwise = Right $ log x

expEither :: Double -> Either ArithmeticError Double
expEither x = Right $ exp x

evalBin :: (Double -> Double -> Either ArithmeticError Double) -> Expr -> Expr -> Either ArithmeticError Double
evalBin op x y = do
  x' <- eval x
  y' <- eval y
  op x' y'

evalUn :: (Double -> Either ArithmeticError Double) -> Expr -> Either ArithmeticError Double
evalUn op x = do
  x' <- eval x
  op x'

-- В Haskell есть do-нотация, которая позволяет выражать то же самое чуть более приятно:
eval :: Expr -> Either ArithmeticError Double
eval (Val n) = return n
eval (UnOp op x) = case op of
  Log -> evalUn logEither x
  Exp -> evalUn expEither x
eval (BinOp op x y) = case op of
  Add -> evalBin addEither x y
  Sub -> evalBin subEither x y
  Mul -> evalBin mulEither x y
  Div -> evalBin divEither x y
  Root -> evalBin rootEither x y

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult = undefined

expr1 :: Expr
expr1 = UnOp Log (Val 0)

expr2 :: Expr
expr2 = UnOp Log (Val (-1))

expr3 :: Expr
expr3 = BinOp Div (Val 1) (Val 0)
