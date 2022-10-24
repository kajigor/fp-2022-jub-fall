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
          | Root Int Expr
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
                     deriving (Show, Eq)

totalDivEither :: Double -> Double -> Either ArithmeticError Double
totalDivEither x y | abs y < 1e-6 = Left DivisionByZero
                   | otherwise = Right $ x / y

totalLogEither :: Double -> Either ArithmeticError Double
totalLogEither x | abs x < 1e-6 = Left LogOfZero
                 | x < 0 = Left LogOfNegativeNumber
                 | otherwise = Right $ log x
totalRootEither :: Int -> Double -> Either ArithmeticError Double
totalRootEither n x | even n && x < 0 =  Left EvenRootOfNegativeNumber
                    | otherwise = Right $ x ** powerOfRoot
                    where
                      powerOfRoot :: Double
                      powerOfRoot = 1 / fromIntegral n

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
eval (Root n x) = do
  x' <- eval x
  totalRootEither n x'



-- Функция принимает на вход результат вычисления арифметического выражения с учетом потенциальных ошибок
-- и генерирует выражения, которые к этому результату вычисляются.
-- Постарайтесь использовать разные конструкторы выражений.
generateExprByResult :: Either ArithmeticError Double -> [Expr]
generateExprByResult (Left DivisionByZero) = [Div lhs rhs | depth <- [1..], numerator <- [minBound :: Int ..],
                                                            lhs <- generateExprByResultWithDepth depth (fromIntegral numerator),
                                                            rhs <- generateExprByResultWithDepth depth 0.0]
generateExprByResult (Left LogOfZero) = [Log lhs | depth <- [1..], lhs <- generateExprByResultWithDepth depth 0.0]
generateExprByResult (Left LogOfNegativeNumber) = [Log lhs | depth <- [1..], value <- [-1.0, -2.0..],
                                                             lhs <- generateExprByResultWithDepth depth value]
generateExprByResult (Left EvenRootOfNegativeNumber) = [Root n lhs | n <- [2,4..], depth <- [1..], value <- [-1.0, -2.0],
                                                                     lhs <- generateExprByResultWithDepth depth value]
generateExprByResult (Right x) = concat [generateExprByResultWithDepth n x | n <- [1..]]

generateExprByResultWithDepth :: Int -> Double -> [Expr] -- Я ограничился +, -, *, - потому что решил, что я не смогу расписать так много вариантов :(
generateExprByResultWithDepth 1 x = [Val x]
generateExprByResultWithDepth n x = generatePlus ++ generateMinus ++ generateDiv ++ generateMul
  where
    depthRange :: [Int]
    depthRange = [1..n-1]

    possibleDepthsSub :: [(Int, Int, Double)]
    possibleDepthsSub = [(i, j, value) | i <- depthRange, j <- depthRange, value <-[x - 100, x - 99..x + 100]] ++
                     [(j, i, value) | i <- depthRange, j <- depthRange, value <-[x - 100, x - 99..x + 100]]
    possibleDepthsMulDiv :: [(Int, Int, Double)]
    possibleDepthsMulDiv = [(i, j, value) | i <- depthRange, j <- depthRange, value <-[0.01, 0.02..100]] ++
                     [(j, i, value) | i <- depthRange, j <- depthRange, value <-[0.01, 0.02..100]]
    justNumbers :: [(Int, Int, Double)]
    justNumbers = [(i, j, value) | i <- depthRange, j <- depthRange, value <-[1, 2..100]] ++
                  [(j, i, value) | i <- depthRange, j <- depthRange, value <-[1, 2..100]]


    genericHelper :: (Expr -> Expr -> Expr) -> (Double -> Double) -> ((Int, Int, Double) -> [Expr])
    genericHelper constructor rightValue = \(a, b, c) -> [constructor lhs rhs |
                                                          lhs <- generateExprByResultWithDepth a c,
                                                          rhs <- generateExprByResultWithDepth b (rightValue c)]

    generatePlus :: [Expr]
    generatePlus = concatMap (genericHelper Add (x-))  possibleDepthsSub

    generateMinus :: [Expr]
    generateMinus = concatMap (genericHelper Sub (\a -> a - x)) possibleDepthsSub

    generateMul :: [Expr]
    generateMul | x == 0 = concatMap (genericHelper Mul $ const 0) justNumbers
                | otherwise = concatMap (genericHelper Mul (x/)) possibleDepthsMulDiv
    generateDiv :: [Expr]
    generateDiv | x == 0 = concatMap (genericHelper (flip Div) $ const 0) justNumbers
                | otherwise = concatMap (genericHelper Div (/x)) possibleDepthsMulDiv

