module Main (main) where

import Expr (ArithmeticError (DivisionByZero, EvenSqrtOfNeg, LogOfNegativeNumber, LogOfZero), generateExprByResult)
import GHC.IO.Encoding.UTF32 (utf32be_decode)

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

errorList :: Int -> String -> String
errorList num errtype
  | errtype == "divzero" = show (take num . generateExprByResult $ Left DivisionByZero)
  | errtype == "logneg" = show (take num . generateExprByResult $ Left LogOfNegativeNumber)
  | errtype == "logzero" = show (take num . generateExprByResult $ Left LogOfZero)
  | errtype == "sqrtneg" = show (take num . generateExprByResult $ Left EvenSqrtOfNeg)
  | otherwise = undefined

getValue :: IO ()
getValue = do
  putStrLn "\nHow many expressions do you need?"
  numOfExpr <- getLine
  let parsedNumOfExpr = read numOfExpr :: Int
  putStrLn "Value (v) or Error (e)?"
  option <- getLine
  if option == "e"
    then do putStrLn "\nDivisionByZero (divzero), LogOfNegativeNumber (logneg), LogOfZero (logzero) or EvenSqrtOfNeg (sqrtneg)?"; o <- getLine; putStrLn (errorList parsedNumOfExpr o)
    else do
      putStrLn "\nTo what value are the expressions needed?"
      value <- getLine
      let parsedValue = read value :: Double
      print (take parsedNumOfExpr (generateExprByResult $ Right parsedValue))

-- -- Спрашиваем имя у пользователя и приветствуем его
-- --greetByName :: IO ()
-- greetByName = do
--   putStrLn "\nWhat's your name?"
--   name <- getLine
--   putStrLn $ "Hello, " ++ name

-- -- Спрашиваем у пользователя его любимое число, парсим его, используя стандартный read.
-- -- Эта функция возвращает полученного число как результат, который мы дальше, в main, сможешь получить и использовать.
-- -- Вывод и ввод данных тут является эффектом, число -- результатом, что отражается в типе функции :: IO Int
-- askFavoriteNumber :: IO Int
-- askFavoriteNumber = do
--   putStrLn "What's your favorite number?"
--   num <- getLine
--   -- read может завершиться ошибкой, которую мы не будем обрабатывать.
--   let parsedNum = read num :: Int
--   -- Возвращаем результат
--   return parsedNum

-- -- Функции с IO -- полноценные функции.
-- -- У них могут быть не только возвращаемые значения, но и аргументы.
-- -- Например, тут мы реагируем на любимое число пользователя.
-- -- Функция принимает любимое число программы, и дальше использует его в своей реакции.
-- reactToNumber :: Int -> IO ()
-- reactToNumber program'sFavoriteNumber = do
--   person'sFavoriteNumber <- askFavoriteNumber
--   if person'sFavoriteNumber == program'sFavoriteNumber
--   then
--     putStrLn "Wow! I have the same favorite number!"
--   else do
--     let doubled = person'sFavoriteNumber * 2
--     putStrLn $ "Nice! BTW, " ++ show person'sFavoriteNumber ++ " doubled is " ++ show doubled

-- Лаконичный main.
main :: IO ()
main = do
  getValue

--greetByName
--reactToNumber 42

-- Эта функция просто демонстрирует, во что дешугарится do-нотация
f :: IO ()
f = do
  x <- getLine -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine -- getLine >>= \y ->
  putStrLn (y ++ y) -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2) -- print (parsed * 2) >>= \r ->
  -- return r
