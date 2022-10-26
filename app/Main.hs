module Main (main) where
import Expr

-- IO -- монада для операций ввода-вывода.
-- Функция-точка входа в программу Main.main имеет тип IO (),
-- это означает, что она не возвращает никакого осмысленного результата, но при этом делает некоторые side effects.
-- Примерами эффектов может быть считывание строки со стандартного входа, печать в стандартный выход,
-- чтение и запись файлов.
-- Текущая программа взаимодействует с пользователем, спрашивая его имя и любимое число.
-- При работе с IO удобно пользоваться do-нотацией.
-- main :: IO ()
-- main = do
--   putStrLn "\nWhat's your name?"
--   x <- getLine
--   putStrLn $ "Hello, " ++ x
--
--   putStrLn "What's your favorite number?"
--   num <- getLine
--   let favNum = read num :: Int
--
--   let program'sFavoriteNumber = 42
--
--   if favNum == program'sFavoriteNumber
--   then
--     putStrLn "Wow! I have the same favorite number!" :: IO ()
--   else do
--     let doubled = favNum * 2
--     putStrLn $ "Nice! BTW, " ++ show favNum ++ " doubled is " ++ show doubled :: IO ()

-- Этот же код можно переписать чуть более аккуратно, заведя функции для запросов.

-- Просто выводим строку на выход.
introduction :: IO ()
introduction = do
  putStrLn "\nHi! I'm a sample application"

-- Спрашиваем имя у пользователя и приветствуем его
greetByName :: IO ()
greetByName = do
  putStrLn "\nWhat's your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name

-- Спрашиваем у пользователя его любимое число, парсим его, используя стандартный read.
-- Эта функция возвращает полученного число как результат, который мы дальше, в main, сможешь получить и использовать.
-- Вывод и ввод данных тут является эффектом, число -- результатом, что отражается в типе функции :: IO Int
askFavoriteNumber :: IO Int
askFavoriteNumber = do
  putStrLn "What's your favorite number?"
  num <- getLine
  -- read может завершиться ошибкой, которую мы не будем обрабатывать.
  let parsedNum = read num :: Int
  -- Возвращаем результат
  return parsedNum

-- Функции с IO -- полноценные функции.
-- У них могут быть не только возвращаемые значения, но и аргументы.
-- Например, тут мы реагируем на любимое число пользователя.
-- Функция принимает любимое число программы, и дальше использует его в своей реакции.
reactToNumber :: Int -> IO ()
reactToNumber program'sFavoriteNumber = do
  person'sFavoriteNumber <- askFavoriteNumber
  if person'sFavoriteNumber == program'sFavoriteNumber
  then
    putStrLn "Wow! I have the same favorite number!"
  else do
    let doubled = person'sFavoriteNumber * 2
    putStrLn $ "Nice! BTW, " ++ show person'sFavoriteNumber ++ " doubled is " ++ show doubled

intro :: IO()
intro = putStrLn "\nThis program will create expressions, which evaluates to a given number."

askForANumber :: IO Double
askForANumber = do
  putStrLn "What should be the resulting value of expressions?"
  num <- getLine
  let parsedNum = read num :: Double
  return parsedNum

askForNumberOfExpr :: IO Int
askForNumberOfExpr = do
  putStrLn "How many expressions do you want to get?"
  num <- getLine
  let parsedNum = read num :: Int
  return parsedNum

printNeededValidExpr :: IO()
printNeededValidExpr = do
  x <- askForANumber
  n <- askForNumberOfExpr
  print $ take n $ generateExprByResult (Right x)

askForATypeOfExpr :: IO String
askForATypeOfExpr = do
  putStrLn "Do you want to have a valid expression? Answer y/n"
  getLine

askForATypeOfAnError :: IO String
askForATypeOfAnError = do
  putStrLn "What error do you want your expresstion to evaluate to?"
  getLine

-- data ArithmeticError = DivisionByZero
--                      | LogOfZero
--                      | LogOfNegativeNumber
--                      | SqrtOfNegativeNumber
--                      | SqrtOfNonPositiveDegree

printNeededInvalidExpr :: IO()
printNeededInvalidExpr = do
  x <- askForATypeOfAnError
  let errorType | x == "DivisionByZero" = DivisionByZero
                | x == "LogOfZero" = LogOfZero
                | x == "LogOfNegativeNumber" = LogOfNegativeNumber
                | x == "SqrtOfNegativeNumber" = SqrtOfNegativeNumber
                | otherwise = SqrtOfNonPositiveDegree
  n <- askForNumberOfExpr
  print $ take n $ generateExprByResult (Left errorType)

app :: IO()
app = do
  exprType <- askForATypeOfExpr
  if exprType == "y" then
    printNeededValidExpr
  else
    printNeededInvalidExpr

-- Лаконичный main.
main :: IO ()
main = do
  intro
  app
  -- introduction
  -- greetByName
  -- reactToNumber 42


-- Эта функция просто демонстрирует, во что дешугарится do-нотация
f :: IO ()
f = do
  x <- getLine        -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine        -- getLine >>= \y ->
  putStrLn (y ++ y)   -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2)  -- print (parsed * 2) >>= \r ->
                      -- return r
