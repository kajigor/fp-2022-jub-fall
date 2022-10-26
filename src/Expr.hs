module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Mult Expr Expr
          | Sum Expr Expr
          | Sub Expr Expr
          | PowE Expr
          | Root Expr Expr
          | Log Expr
          deriving (Show, Eq)

-- Пример выражения в нашем абстрактном синтаксисе
-- (12 / log 10) / 3
expr :: Expr
expr = Div (Div (Val 12) (Log (Val 10))) (Val 3)

expr1stRootFrom0 :: Expr
expr1stRootFrom0 = Root (Val 1) (Val 0)

exprPowE :: Expr
exprPowE = PowE (Log (Val 1))

expr3rdRootFrom27 :: Expr
expr3rdRootFrom27 = Root (Val 3) (Val 27)

exprNegativeRootDegree :: Expr
exprNegativeRootDegree = Root (Val (-3)) (Val 27)

exprRoot :: Expr
exprRoot = Root (Val 2) (Val 16)

exprComplex :: Expr
exprComplex = Sub (Sum (Mult (Div (Div (Val 12) (Log (Val 10))) (Val 3)) (Val 2)) (Val 10)) (Val 99)

-- Выражение, вычисление которого должно приводить к ошибке.
-- Содержит деление на 0 и логарифм от отрицательного числа.
-- (6 / log (-1)) / 0
exprErr :: Expr
exprErr = Div (Div (Val 6) (Log (Val (-1)))) (Val 0)

exprErrLogFromZero :: Expr
exprErrLogFromZero = Log (Val 0)

exprErrLogFromNegativeNumber :: Expr
exprErrLogFromNegativeNumber = Log (Val (-1))

exprErrDivisionByZero :: Expr
exprErrDivisionByZero = Div (Val 1) (Val 0)

exprErr0thRoot :: Expr
exprErr0thRoot = Root (Val 0) (Val 27)

exprErrRootNegativeBase :: Expr
exprErrRootNegativeBase = Root (Val 3) (Val (-27))

-- В случае ошибки теперь мы явно возвращаем Nothing,
-- а в случае успеха результат вычисления выражения заворачивается в конструктор Just
-- *Expr> evalMaybe expr
-- Just 1.737177927613007
-- *Expr> evalMaybe exprErr
-- Nothing

-- Что если мы хотим предоставлять пользователю информативное сообщение об ошибке?
-- В этом случае имеет смысл воспользоваться типом данных Either e a = Left e | Right a.
-- Конструктор Right заворачивает корректное значение, в то время как Left -- ошибку.
-- Также мы можем создать специальный тип для ошибки ArithmeticError, чтобы явно
-- различать, какая именно произошла ошибка: деление на 0 или логарифм от неположительного числа

data ArithmeticError = DivisionByZero
                     | LogOfZero
                     | LogOfNegativeNumber
                     | DegreeOfZero
                     | RootNegativeBase
                     deriving (Show, Eq)

isArithmeticErrorExpected :: String -> Bool
isArithmeticErrorExpected input = input `elem` [
  "DivisionByZero",
  "LogOfZero",
  "LogOfNegativeNumber",
  "DegreeOfZero",
  "RootNegativeBase"
  ]

convertStrToArithmeticError :: String -> ArithmeticError
convertStrToArithmeticError e | e == "DivisionByZero" = DivisionByZero
                              | e == "LogOfZero" = LogOfZero
                              | e == "LogOfNegativeNumber" = LogOfNegativeNumber
                              | e == "DegreeOfZero" = DegreeOfZero
                              | e == "RootNegativeBase" = RootNegativeBase
                              | otherwise = undefined

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalMultEither :: Double -> Double -> Either ArithmeticError Double
totalMultEither x y = Right $ x * y

totalSumEither :: Double -> Double -> Either ArithmeticError Double
totalSumEither x y = Right $ x + y

totalSubEither :: Double -> Double -> Either ArithmeticError Double
totalSubEither x y = Right $ x - y

totalPowEEither :: Double -> Either ArithmeticError Double
totalPowEEither x = Right $ exp 1 ** x

totalRootEither :: Double -> Double -> Either ArithmeticError Double
totalRootEither x y | x == 0 = Left DegreeOfZero
                    | y < 0 = Left RootNegativeBase
                    | otherwise = Right $ y ** (1 / x)

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
eval (Mult x y) = do
  x' <- eval x
  y' <- eval y
  totalMultEither x' y'
eval (Sum x y) = do
  x' <- eval x
  y' <- eval y
  totalSumEither x' y'
eval (Sub x y) = do
  x' <- eval x
  y' <- eval y
  totalSubEither x' y'
eval (PowE x) = do
  x' <- eval x
  totalPowEEither x'
eval (Root x y) = do
  x' <- eval x
  y' <- eval y
  totalRootEither x' y'

-- Генерирует одно выражение, вычисление которго приводит к указанной ошибке
generateExprByError :: ArithmeticError -> Expr -> Expr
generateExprByError DivisionByZero e = Div e (Val 0)
generateExprByError LogOfNegativeNumber e = Log (Mult (Val (-1)) e)
generateExprByError LogOfZero _ = Log (Val 0)
generateExprByError DegreeOfZero e = Root (Val 0) e
generateExprByError RootNegativeBase e = Root e (Val (-1))

getRemainderDouble :: Double -> Int -> Int
getRemainderDouble v d = (round v :: Int) `mod` d

getRemainder :: Int -> Int -> Int
getRemainder v d = v `mod` d

generateAdditionalValue :: Double -> Double
generateAdditionalValue v
  | getRemainderDouble v 10 == 1 = v + 1
  | getRemainderDouble v 10 == 2 = v - 4
  | getRemainderDouble v 10 == 3 = v * 2
  | getRemainderDouble v 10 == 4 = v / 7
  | getRemainderDouble v 10 == 5 = v + 3
  | getRemainderDouble v 10 == 6 = v - 5
  | getRemainderDouble v 10 == 7 = v * 6
  | getRemainderDouble v 10 == 8 = v / 9
  | getRemainderDouble v 10 == 9 = v + 8
  | otherwise = (v / 3) ** 2

getValueOrDefaultIfZero :: Double -> Double -> Double
getValueOrDefaultIfZero v d = if v == 0 then d else v

generateValExpr :: Double -> Expr
generateValExpr = Val

generateDivExpr :: Double -> Expr
generateDivExpr a =
  let safeB = getValueOrDefaultIfZero (generateAdditionalValue a) 2 in
    Div (Val (a * safeB)) (Val safeB)

generateMultExpr :: Double -> Expr
generateMultExpr a =
  let safeB = getValueOrDefaultIfZero (generateAdditionalValue a) 3 in
    if a == 0 
      then Mult (Val a) (Val 1)
      else Mult (Val (a / safeB)) (Val safeB)

generateSumExpr :: Double -> Expr
generateSumExpr a =
    let b = generateAdditionalValue a in
      Sum (Val (a - b)) (Val b)

generateSubExpr :: Double -> Expr
generateSubExpr a =
    let b = generateAdditionalValue a in
      Sub (Val (a + b)) (Val b)

generateRootExpr :: Double -> Expr
generateRootExpr a =
    let safeB = getValueOrDefaultIfZero (generateAdditionalValue a) 4 in
    if a <= 0 
      then Sum (Root (Val safeB) (Val (17 ** safeB))) (Val (a - 17)) 
      else Root (Val safeB) (Val (a ** safeB))

generatePowELogExpr :: Double -> Expr
generatePowELogExpr a = 
  if a <= 0 
    then Sum (PowE (Log (generateValExpr 42))) (generateValExpr (a - 42)) 
    else PowE (Log (generateValExpr a))

-- Генерериет выражение, которое вычисляется к указанному значению
generateExprByValue :: Double -> Expr
generateExprByValue v
  | getRemainderDouble v 7 == 1 = generateValExpr v
  | getRemainderDouble v 7 == 2 = generateDivExpr v
  | getRemainderDouble v 7 == 3 = generateMultExpr v
  | getRemainderDouble v 7 == 4 = generateSumExpr v
  | getRemainderDouble v 7 == 5 = generateSubExpr v
  | getRemainderDouble v 7 == 6 = generateRootExpr v
  | otherwise = generatePowELogExpr v

generateExprByStepAndValue :: Int -> Double -> Expr
generateExprByStepAndValue s v 
  | getRemainder s 6 == 1 = Sum (generateExprByValue (v - fromIntegral s)) (generateValExpr (fromIntegral s))
  | getRemainder s 6 == 2 = Sub (generateExprByValue (v + fromIntegral s)) (generateValExpr (fromIntegral s))
  | getRemainder s 6 == 3 = Mult (generateExprByValue (v / fromIntegral s)) (generateValExpr (fromIntegral s))
  | getRemainder s 6 == 4 = Div (Val (v * fromIntegral s)) (generateExprByValue (fromIntegral s))
  | getRemainder s 6 == 5 = 
    if v <= 0
      then Sum (Root (Val (fromIntegral s)) (Val (56 ** fromIntegral s))) (Val (v - 56)) 
      else Root (Val (fromIntegral s)) (Val (v ** fromIntegral s))
  | otherwise = generatePowELogExpr v

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> Int -> [Expr]
generateExprByResult result 1 =
  case result of
    Right v -> [generateExprByValue v]
    Left e -> [generateExprByError e (generateExprByValue 1)]
generateExprByResult result n =
  case result of
    Right v -> generateExprByStepAndValue n v : generateExprByResult result (n-1)
    Left e -> generateExprByError e (generateExprByValue (fromIntegral n)) : generateExprByResult result (n-1)
