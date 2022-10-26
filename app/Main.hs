module Main (main) where

import Expr
{-
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

-}


is_error :: IO Bool 
is_error = do
  putStrLn "\nEnter error if you want to terminate with a error or number in other case"
  ans <- getLine
  if ans == "error" then return True else return False

readError :: IO ArithmeticError
readError = do
  putStrLn "\nEnter error type"
  errorType <- getLine
  case errorType of
    "Division by zero" -> return DivisionByZero
    "Log of zero" -> return LogOfZero
    "Log of negative number" -> return LogOfNegativeNumber
    _ -> return SqrtOfNegativeNumber


main :: IO ()
main = do
  type_input <- is_error
  result <- if type_input 
  then do
    error_name <- readError
    return $ Left error_name
  else do
    result_num <- readDouble
    return $ Right result_num
  expr_num <- readInt
  mapM_ print $ take expr_num $  generateExprByResult result

readDouble :: IO Double
readDouble = do
  putStrLn "\nEnter result number"
  num <- getLine
  let parse = read num :: Double
  return parse 

readInt :: IO Int
readInt = do
  putStrLn "\nEnter amount of expression"
  num <- getLine
  let parse = read num :: Int
  return parse


-- Эта функция просто демонстрирует, во что дешугарится do-нотация
{-f :: IO ()
f = do
  x <- getLine        -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine        -- getLine >>= \y ->
  putStrLn (y ++ y)   -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2)  -- print (parsed * 2) >>= \r ->
 -}                     -- return r
