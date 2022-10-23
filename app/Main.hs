module Main (main) where

import Text.Read
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

-- the idea is taken from here:
-- https://stackoverflow.com/questions/33969601/haskell-keep-adding-user-input-ints-until-a-negative-is-encountered
askUntil :: String -> (String -> Bool) -> IO String
askUntil message predicate = do
  putStrLn message
  value <- getLine
  if predicate value
    then return value
    else askUntil message predicate

askTargetValue :: IO (Either ArithmeticError Double)
askTargetValue = do
  putStrLn "Do you want to enter a floating point number or to select and arithmetic error?"
  option <- askUntil "Possible options: 'number', 'error'" (\x -> x == "number" || x == "error")
  case option of
    "number" -> do
      number <- askUntil "Please, enter the target number:" (\x -> (readMaybe x :: Maybe Double) /= Nothing)
      return (Right (read number :: Double))
    "error" -> do
      err <- askUntil "Please, enter the target arithmetic error (one of DivisionByZero, LogOfZero, LogOfNegativeNumber, SqrtOfNegativeNumber):" (\x -> x == "DivisionByZero" || x == "LogOfZero" || x == "LogOfNegativeNumber" || x == "SqrtOfNegativeNumber")
      case err of
        "DivisionByZero" -> return (Left DivisionByZero)
        "LogOfZero" -> return (Left DivisionByZero)
        "LogOfNegativeNumber" -> return (Left DivisionByZero)
        "SqrtOfNegativeNumber" -> return (Left DivisionByZero)

isStringPositiveInteger :: String -> Bool
isStringPositiveInteger x = case readMaybe x :: Maybe Int of
  Nothing -> False
  Just n -> if n > 0
    then True
    else False

-- Лаконичный main.
main :: IO ()
main = do
  putStrLn ""
  target <- askTargetValue
  let exprAll = generateExprByResult target
  exprNum <- askUntil "Please, enter how many expressions you want to see. This should be a positive integer" isStringPositiveInteger
  let parsedExprNum = read exprNum :: Int
  putStrLn "Here are the expressions that evaluate to the target value you have entered:"
  mapM_ print (take parsedExprNum $ exprAll)
