{-# OPTIONS_GHC -Wno-type-defaults #-}
module Expr where
import GHC.Float
import Data.Bits (Bits(xor))

-- Тип данных для выражений.
-- Каждое выражение это либо целое число, либо деление двух выражений, либо логарифм другого выражения.
data Expr = Val Double
          | Div Expr Expr
          | Log Expr
          | Sum Expr Expr
          | Sub Expr Expr
          | Exp Expr
          | Sqrt Expr Expr
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
partialEval _ = undefined

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
  maybe Nothing totalLogMaybe (evalMaybe x)
evalMaybe _ = undefined

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
                     | SqrtOfNegativeNumber
                     | SqrtOfNonPositiveDegree
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
evalEither _ = undefined

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | y == 0 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | x == 0 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x

totalSumEither :: Double -> Double -> Either ArithmeticError Double
totalSumEither a b = Right $ a + b

totalSubEither :: Double -> Double -> Either ArithmeticError Double
totalSubEither a b = Right $ a - b

totalExpEither :: Double -> Either ArithmeticError Double
totalExpEither x = Right $ exp x

totalSqrtEither :: Double -> Double -> Either ArithmeticError Double
totalSqrtEither x n | x < 0 = Left SqrtOfNegativeNumber
                    | n <= 0 = Left SqrtOfNonPositiveDegree
                    | otherwise = Right $ powerDouble x (1 / n)

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
  maybe Nothing f value

returnMaybe :: a -> Maybe a
returnMaybe = Just

evalMaybe' :: Expr -> Maybe Double
evalMaybe' (Val v) = returnMaybe v
evalMaybe' (Div x y) =
    evalMaybe' x `bindMaybe` \x' ->
    evalMaybe' y `bindMaybe` \y' ->
    totalDivMaybe x' y'
evalMaybe' (Log x) =
    evalMaybe' x `bindMaybe` \x' ->
    totalLogMaybe x'
evalMaybe' _ = undefined


bindEither :: Either e a -> (a -> Either e b) -> Either e b
bindEither value f =
  case value of
    Right x -> f x
    Left err -> Left err

returnEither :: a -> Either e a
returnEither = Right

evalEither' :: Expr -> Either ArithmeticError Double
evalEither' (Val v) = returnEither v
evalEither' (Div x y) =
    evalEither' x `bindEither` \x' ->
    evalEither' y `bindEither` \y' ->
    totalDivEither x' y'
evalEither' (Log x) =
    evalEither' x `bindEither` \x' ->
    totalLogEither x'
evalEither' _ = undefined

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
evalMaybe'' _ = undefined

evalEither'' :: Expr -> Either ArithmeticError Double
evalEither'' (Val v) = return v
evalEither'' (Div x y) =
    evalEither'' x >>= \x' ->
    evalEither'' y >>= \y' ->
    totalDivEither x' y'
evalEither'' (Log x) =
    evalEither'' x >>= \x' ->
    totalLogEither x'
evalEither'' _ = undefined

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
eval (Sum a b) = do
  a' <- eval a
  b' <- eval b
  totalSumEither a' b'
eval (Sub a b) = do
  a' <- eval a
  b' <- eval b
  totalSubEither a' b'
eval (Exp x) = do
  x' <- eval x
  totalExpEither x'
eval (Sqrt x n) = do
  x' <- eval x
  n' <- eval n
  totalSqrtEither x' n'

-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult (Left DivisionByZero)          = map (`Div` Val 0)    (generateExprByResult $ Right 100)
generateExprByResult (Left LogOfZero)               = map Log              (generateExprByResult $ Right 0)
generateExprByResult (Left LogOfNegativeNumber)     = map Log              (generateExprByResult $ Right $ -100)
generateExprByResult (Left SqrtOfNonPositiveDegree) = map (Sqrt $ Val 100) (generateExprByResult $ Right $ -100)
generateExprByResult (Left SqrtOfNegativeNumber)    = map (`Sqrt` Val 100) (generateExprByResult $ Right $ -100)

generateExprByResult (Right x) = getResult 0 x
  where
    getResult :: Int -> Double -> [Expr]
    getResult acc x' = exprOfDepthByResult acc x' ++ getResult (acc + 1) x'

    exprOfDepthByResult :: Int -> Double -> [Expr]
    exprOfDepthByResult 0 x' = [Val x']
    exprOfDepthByResult n x' = concatMap (\l -> l n x') [getSum, getSub, getExp, getLog, getDiv, getSqrt]

    getSum :: Int -> Double -> [Expr]
    getSum n x' = map (Sum (Val 1)) (exprOfDepthByResult (n - 1) (x' - 1))

    getSub :: Int -> Double -> [Expr]
    getSub n x' = map (`Sub` Val 1) (exprOfDepthByResult (n - 1) (x' + 1))

    getExp :: Int -> Double -> [Expr]
    getExp n x' | x <= 1   = []                       -- если x отрицательный или маленький, то до свидания
                | otherwise = map Exp (exprOfDepthByResult (n - 1) (log x'))

    getLog :: Int -> Double -> [Expr]
    getLog n x' | exp x' > 10 ^ 15 || exp x' < 1 = [] -- если x очень большой или маленький, то не будем дальше
                | otherwise    = map Log $ exprOfDepthByResult (n - 1) (exp x')

    getDiv :: Int -> Double -> [Expr]
    getDiv n x' | abs x' > 10 ^ 15 = []
                | otherwise        = map (`Div` Val 2) (exprOfDepthByResult (n - 1) (2 * x'))

    getSqrt :: Int -> Double -> [Expr]
    getSqrt n x'
      | x' < 0 || x > (10^10) = []                    -- если x отрицательный или уже большой, то не будем корень использовать
      | otherwise             = map (`Sqrt` Val 2) (exprOfDepthByResult (n - 1) (x' ** 2)) 


-- data ArithmeticError = DivisionByZero
--                      | LogOfZero
--                      | LogOfNegativeNumber
--                      | SqrtOfNegativeNumber
--                      | SqrtOfNonPositiveDegree
--                      deriving (Show, Eq)

