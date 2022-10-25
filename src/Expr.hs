module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Exp Expr
          | Root Expr Expr
          deriving (Show, Read, Eq)

-- Что если мы хотим предоставлять пользователю информативное сообщение об ошибке?
-- В этом случае имеет смысл воспользоваться типом данных Either e a = Left e | Right a.
-- Конструктор Right заворачивает корректное значение, в то время как Left -- ошибку.
-- Также мы можем создать специальный тип для ошибки ArithmeticError, чтобы явно
-- различать, какая именно произошла ошибка: деление на 0 или логарифм от неположительного числа

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | RootOfNonpositiveNumber
                     | RootOfDegreeZero
                     deriving (Show, Read, Eq)

type Result = Either ArithmeticError Double

totalDivEither :: Double -> Double -> Result
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Result
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalAddEither :: Double -> Double -> Result
totalAddEither x y = return $ x + y

totalSubEither :: Double -> Double -> Result
totalSubEither x y = return $ x - y

totalExpEither :: Double -> Result
totalExpEither x = return $ exp x

totalRootEither :: Double -> Double -> Result
totalRootEither x y  | x <= 0 = Left RootOfNonpositiveNumber
                    | y == 0 = Left RootOfDegreeZero
                    | otherwise = Right $ exp (log x / y)

-- В Haskell есть do-нотация, которая позволяет выражать то же самое чуть более приятно:
eval :: Expr -> Result
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
  totalAddEither x' y'
eval (Sub x y) = do
  x' <- eval x
  y' <- eval y
  totalSubEither x' y'
eval (Exp x) = do
  x' <- eval x
  totalExpEither x'
eval (Root x y) = do
  x' <- eval x
  y' <- eval y
  totalRootEither x' y'

solveAdd :: Result -> [(Result, Result)]
solveAdd result = case result of
  Right x ->  [(Right (x - a), Right a) | a <- [0..]]
  _ ->  []

solveSub :: Result -> [(Result, Result)]
solveSub result = case result of
  Right x ->  [(Right (x + a), Right a) | a <- [0..]]
  _ ->  []

solveDiv :: Result -> [(Result, Result)]
solveDiv result = case result of
  Right x ->  [(Right (x * a), Right a) | a <- [1..]]
  Left DivisionByZero -> [(Right a, Right 0) | a <- [1..]]
  _ ->  []

solveLog :: Result -> [Result]
solveLog result = case result of
  Right x ->  [Right (exp x) | abs x < 20]
  Left LogOfZero -> [Right 0]
  Left LogOfNegativeNumber -> [Right (-a) | a <- [1..]]
  _ ->  []

solveExp :: Result -> [Result]
solveExp result = case result of
  Right x ->
    [Right (log x) | x > 0]
  _ ->  []

solveRoot :: Result -> [(Result, Result)]
solveRoot result = case result of
  Right x ->
    if x > 0
      then [(Right (x ^ a), Right (fromIntegral a)) | a <- [1..]]
      else []
  Left RootOfNonpositiveNumber -> [(Right (-a), Right b) | a <- [1..], b <- [1..]]
  Left RootOfDegreeZero -> [(Right a, Right 0) | a <- [1..]]
  _ -> []


-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Result -> [Expr]
generateExprByResult result = case result of
    Right value -> Val value : helper result
    _error -> helper result
  where
    helper x = unaryHelper x ++ binaryHelper x
    unaryHelper x = do
      (solve, cons) <- [(solveLog, Log)]
      y <- solve x
      yExpr <- generateExprByResult y
      return $ cons yExpr
    binaryHelper x = do
      (solve, cons) <- [(solveAdd, Add), (solveSub, Sub), (solveDiv, Div)]
      (y, z) <- solve x
      yExpr <- generateExprByResult y
      zExpr <- generateExprByResult z
      return $ cons yExpr zExpr