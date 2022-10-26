module Expr where

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Log Expr
          | Exp Expr
          | Root Expr Integer -- power is a constant
          deriving (Show, Eq)

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
partialEval (Log x) = log (partialEval x)

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
evalMaybe (Div x y) =
  case evalMaybe x of
    -- Проверяем, не произошло ли ошибки при вычислении левого подвыражения
    Just x' ->
      case evalMaybe y of
        -- Проверяем, не произошло ли ошибки при вычислении правого подвыражения
        Just y' -> totalDivMaybe x' y'
        -- В случае ошибки можем только сообщать об ошибке
        Nothing -> Nothing
    Nothing -> Nothing
evalMaybe (Log x) =
  case evalMaybe x of
    Just x' -> totalLogMaybe x'
    Nothing -> Nothing

totalDivMaybe :: Double -> Double -> Maybe Double
totalDivMaybe x y | y == 0 = Nothing
                  | otherwise = Just $ x / y

totalLogMaybe :: Double -> Maybe Double
totalLogMaybe x | x <= 0 = Nothing
                | otherwise = Just $ log x

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
                     | EvenRootOfNegativeNumber
                     | NonPositiveRoot
                     deriving (Show, Eq)

evalEither :: Expr -> Either ArithmeticError Double
-- Этот интерпретатор будет возвращать Nothing в случае деления на 0
-- или вычисления логарифма от неположительного числа
evalEither (Val v) = Right v
evalEither (Div x y) =
    case evalEither x of
      Right x' ->
        case evalEither y of
          Right y' -> totalDivEither x' y'
          Left err -> Left err
      Left err  -> Left err
evalEither (Log x) =
    case evalEither x of
      Right x' -> totalLogEither x'
      Left err -> Left err

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalAddEither :: Double -> Double -> Either ArithmeticError Double
totalAddEither x y  = Right (x + y) -- we assume that addition always works correctly

totalSubEither :: Double -> Double -> Either ArithmeticError Double
totalSubEither x y  = Right (x - y) -- we assume that subtraction always works correctly

totalMulEither :: Double -> Double -> Either ArithmeticError Double
totalMulEither x y  = Right (x * y) -- we assume that multiplication always works correctly

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalRootEither :: Double -> Integer -> Either ArithmeticError Double
totalRootEither x p | p <= 0 = Left NonPositiveRoot
                    | even p && x < 0 = Left EvenRootOfNegativeNumber
                    | otherwise = Right (x ** (1 / fromIntegral p))

totalExpEither :: Double -> Either ArithmeticError Double
totalExpEither x = Right (exp x) -- we assume that exponent always works correctly

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

-- В коде evalMaybe и evalEither очень много повторяющейся логики: мы проверяем, что какое-то подвыражение
-- вычисляется без ошибок, и если это так, то продолжаем вычисление выражения.
-- Если происходит ошибка, мы о ней сообщаем.
-- Эту повторяющуюся логику было бы неплохо абстрагировать, что мы можем сделать,
-- используя bindMaybe, returnMaybe, bindEither и returnEither.

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe value f =
  case value of
    Just x -> f x
    Nothing -> Nothing

returnMaybe :: a -> Maybe a
returnMaybe x = Just x

evalMaybe' :: Expr -> Maybe Double
evalMaybe' (Val v) = returnMaybe v
evalMaybe' (Div x y) =
    evalMaybe' x `bindMaybe` \x' ->
    evalMaybe' y `bindMaybe` \y' ->
    totalDivMaybe x' y'
evalMaybe' (Log x) =
    evalMaybe' x `bindMaybe` \x' ->
    totalLogMaybe x'


bindEither :: Either e a -> (a -> Either e b) -> Either e b
bindEither value f =
  case value of
    Right x -> f x
    Left err -> Left err

returnEither :: a -> Either e a
returnEither x = Right x

evalEither' :: Expr -> Either ArithmeticError Double
evalEither' (Val v) = returnEither v
evalEither' (Div x y) =
    evalEither' x `bindEither` \x' ->
    evalEither' y `bindEither` \y' ->
    totalDivEither x' y'
evalEither' (Log x) =
    evalEither' x `bindEither` \x' ->
    totalLogEither x'

-- Класс типов Monad предоставляет функции >>= (bind) и return ровно с тем функционалом, который нам необходим.
-- Monad и Either e являются инстансами класса Monad, поэтому мы можем использовать функции из него:

evalMaybe'' :: Expr -> Maybe Double
evalMaybe'' (Val v) = return v
evalMaybe'' (Div x y) =
    evalMaybe'' x >>= \x' ->
    evalMaybe'' y >>= \y' ->
    totalDivMaybe x' y'
evalMaybe'' (Log x) =
    evalMaybe'' x `bindMaybe` \x' ->
    totalLogMaybe x'

evalEither'' :: Expr -> Either ArithmeticError Double
evalEither'' (Val v) = return v
evalEither'' (Div x y) =
    evalEither'' x >>= \x' ->
    evalEither'' y >>= \y' ->
    totalDivEither x' y'
evalEither'' (Log x) =
    evalEither'' x >>= \x' ->
    totalLogEither x'

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
eval (Mul x y) = do
  x' <- eval x
  y' <- eval y
  totalMulEither x' y'
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
eval (Root x p) = do
  x' <- eval x
  totalRootEither x' p


-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult (Right val) = map gen [1..]
  where gen :: Integer -> Expr
        gen 0 = Val val
        gen x | even x = Div (Mul (gen (x - 1)) (Val 5)) (Val 5)
        gen x | x `mod` 3 == 0 = Sub (Add (gen (x - 1)) (Val 0.5)) (Val 0.5)
        gen x = Log (Exp (gen (x - 1)))
generateExprByResult (Left DivisionByZero) = map (\n -> Div (Val n) (Val 0)) [1 ..]
generateExprByResult (Left LogOfZero) = map (\n -> Log (Sub (Val n) (Val n))) [1 ..]
generateExprByResult (Left LogOfNegativeNumber) = map (Log . Sub (Val 0) . Val) [1 ..]
generateExprByResult (Left NonPositiveRoot) = map (\n -> Root (Val n) (round (negate n))) [1 ..]
generateExprByResult (Left EvenRootOfNegativeNumber) = map (\n -> Root (Val (negate n)) (round (2*n))) [1 ..]
