module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Exp Expr
          | Root Expr Expr
          deriving (Show, Eq)

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | RootOfNonpositiveNumber
                     | RootOfZeroDegree
                     deriving (Show, Read, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalRootEither :: Double -> Double -> Either ArithmeticError Double
totalRootEither x d | d == 0 = Left RootOfZeroDegree
                    | x <= 0 = Left RootOfNonpositiveNumber
                    | otherwise = Right $ exp $ (log x) / d

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
  x' <- eval x
  y' <- eval y
  Right $ x' + y'
eval (Sub x y) = do
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
eval (Root x d) = do
  x' <- eval x
  d' <- eval d
  totalRootEither x' d'

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult result = generateExprByResultAndNumber result 10

generateExprByResultAndNumber :: Either ArithmeticError Double -> Int -> [Expr]
generateExprByResultAndNumber result number 
  | number <= 0 = []
  | otherwise = (generateOneExprByResult result number) ++ (generateExprByResultAndNumber result (number - 1))
  where 
    generateOneExprByResult :: Either ArithmeticError Double -> Int -> [Expr]
    generateOneExprByResult result seed = case result of
      Left DivisionByZero -> [Div (generate seed) (generateZero seed)]
      Left LogOfZero -> [Log $ generateZero seed]
      Left LogOfNegativeNumber -> [Log $ generate (-1)]
      Left RootOfNonpositiveNumber -> [Root (generate (-seed)) (generate seed), Root (generateZero seed) (generate $ seed * 2)]
      Left RootOfZeroDegree -> [Root (generate seed) (generateZero seed)]
      Right x -> [Mul (generate 1) (Val x),
                  Add (Val $ x - 1) (generate 1), 
                  Mul (generate (-1)) (Sub (generateZero seed) (Val x))]

    generate :: Int -> Expr
    generate x 
      | x == 0 = generateZero 5
      | x < 0 =
        if odd x
          then Sub (generateZero x) (generate (-x))
          else Mul (generate (-x)) (generate (-1))
      | otherwise = 
        case x `mod` 5 of
          0 -> Exp (Log (Val $ fromIntegral x))
          1 -> Val $ fromIntegral x 
          2 -> Mul (generate $ -1) (Sub (generateZero x) (Add (generate (x - 1)) (generate 1)))
          3 -> Div (generate (x * 2)) (generate 2)
          4 -> Root (generate (x * x)) (generate 2)

    generateZero :: Int -> Expr
    generateZero seed = case seed `mod` 3 of
      0 -> Val 0
      1 -> Log $ Val 1
      2 -> Mul (generateZero (seed - 1)) (Val $ fromIntegral seed)
      