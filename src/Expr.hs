module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Exp Expr
          | Root Expr Integer
          deriving (Read, Show, Eq)

-- Пример выражения в нашем абстрактном синтаксисе
-- (12 / log 10) / 3
expr :: Expr
expr = Div (Div (Val 12) (Log (Val 10))) (Val 3)

-- Выражение, вычисление которого должно приводить к ошибке.
-- Содержит деление на 0 и логарифм от отрицательного числа.
-- (6 / log (-1)) / 0
exprErr :: Expr
exprErr = Div (Div (Val 6) (Log (Val (-1)))) (Val 0)

-- Частично определенный интерпретатор выражений.
partialEval :: Expr -> Double
partialEval (Val v) = v
partialEval (Div x y) = partialEval x / partialEval y
partialEval (Log (Exp x)) = partialEval x
partialEval (Exp (Log x)) | arg > 0 = arg where arg = partialEval x
partialEval (Exp x) = exp $ partialEval x
partialEval (Log x) = log $ partialEval x
partialEval (Root x y) = partialEval x ** (1 / fromIntegral y)

-- Вычисление корректного выражения с нормальным результатом
-- *Expr> partialEval expr
-- 1.737177927613007

-- Вычисление выражения с делением на 0 приводит к результату NaN.
-- Хоть это и не исключение, пробрасываемое до пользователя, это все же ошибка,
-- которую хочется явно отлавливать и цивилизованно сообщать о ней пользователю.
-- *Expr> partialEval exprErr
-- NaN

-- Этот интерпретатор будет возвращать Nothing в случае деления на 0
-- или вычисления логарифма от неположительного числа
evalMaybe :: Expr -> Maybe Double
evalMaybe (Val v) = Just v
evalMaybe (Div x y) = case (evalMaybe x, evalMaybe y) of
    (Just x', Just y') -> totalDivMaybe x' y'
    _ -> Nothing
evalMaybe (Log (Exp x)) = evalMaybe x
evalMaybe (Exp (Log x)) | arg > 0 = Just arg where Just arg = evalMaybe x
evalMaybe (Log x) = maybe Nothing totalLogMaybe (evalMaybe x)
evalMaybe (Exp x) = maybe Nothing totalExpMaybe (evalMaybe x)
evalMaybe (Root x y) = case evalMaybe x of
    Just x' -> if x' >= 0 && y /= 0 || odd y then Just $ x' ** (1 / fromIntegral y) else Nothing
    _ -> Nothing

totalDivMaybe :: Double -> Double -> Maybe Double
totalDivMaybe x y | y == 0 = Nothing
                  | otherwise = Just $ x / y

totalLogMaybe :: Double -> Maybe Double
totalLogMaybe x | x <= 0 = Nothing
                | otherwise = Just $ log x

totalExpMaybe :: Double -> Maybe Double
totalExpMaybe x = Just $ log x

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
                     | EvenRootOfNegBase
                     | ZeroRoot
                     deriving (Read, Show, Eq)

eval :: Expr -> Either ArithmeticError Double
-- Этот интерпретатор будет возвращать Nothing в случае деления на 0
-- или вычисления логарифма от неположительного числа
eval (Val v) = Right v
eval (Div x y) = do
  x' <- eval x
  y' <- eval y
  totalDivEither x' y'
eval (Log (Exp x)) = eval x
eval (Exp (Log x)) | arg > 0 = Right arg where Right arg = eval x
eval (Log x) = do
  x' <- eval x
  totalLogEither x'
eval (Exp x) = do
  x' <- eval x
  Right $ exp x'
eval (Root x y) = do
  x' <- eval x
  totalRootEither x' y

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalRootEither :: Double -> Integer -> Either ArithmeticError Double
totalRootEither x y | y == 0 = Left ZeroRoot
                    | even y && x < 0 = Left EvenRootOfNegBase
                    | otherwise = Right $ x ** (1 / fromIntegral y)

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

expr1 :: Expr
expr1 = Log (Val 0)

expr2 :: Expr
expr2 = Log (Val (-1))

expr3 :: Expr
expr3 = Div (Val 1) (Val 0)

-- *Expr> evalEither expr
-- Right 1.737177927613007
-- *Expr> evalEither expr1
-- Left LogOfZero
-- *Expr> evalEither expr2
-- Left LogOfNegativeNumber
-- *Expr> evalEither expr3
-- Left DivisionByZero

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.

nat :: [Double]
nat = [1..]

id1 :: Double -> Expr
id1 = Val

id2 :: Expr -> Expr
id2 x = Log (Exp x)

id3 :: Expr -> Expr
id3 x = Root (Div (Val 1) x) (-1)

idList :: [Expr -> Expr]
idList = id2 : id3 : [x . y | x <- idList, y <- idList]

generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult (Right v) = Val v : map f idList where f n = n (Val v)
generateExprByResult (Left DivisionByZero) = map f nat where f n = Div (Val n) (Val 0)
generateExprByResult (Left LogOfZero) = repeat $ Log (Val 0)
generateExprByResult (Left LogOfNegativeNumber) = map f nat where f n = Log (Val $ -n)
generateExprByResult (Left EvenRootOfNegBase) = map f nat where f n = Root (Val $ -n) 2
generateExprByResult (Left ZeroRoot) = map f nat where f n = Root (Val n) 0

