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

-- Эта функция просто демонстрирует, во что дешугарится do-нотация
f :: IO ()
f = do
  x <- getLine        -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine        -- getLine >>= \y ->
  putStrLn (y ++ y)   -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2)  -- print (parsed * 2) >>= \r ->
                      -- return r





getNum :: Double -> Int -> IO ()
getNum _ 0 = return ()
getNum result amount = do
  print $ generateExprByResult $ Right result
  getNum result (amount - 1)

getError :: Int -> Int -> IO ()
getError _ 0 = return ()
getError err amount = do
  case err of
    2 -> print $ generateExprByResult $ Left DivisionByZero
    3 -> print $ generateExprByResult $ Left LogOfZero
    4 -> print $ generateExprByResult $ Left LogOfNegativeNumber
    5 -> print $ generateExprByResult $ Left SqrtOfNegativeNumber
    6 -> print $ generateExprByResult $ Left ZeroRoot
  getError err (amount - 1)

getResType :: IO Int
getResType = do
  putStrLn "Please, select the value to which the expression should be evaluated:"
  putStrLn "  1. Number"
  putStrLn "  2. DivisionByZero"
  putStrLn "  3. LogOfZero"
  putStrLn "  4. LogOfNegativeNumber"
  putStrLn "  5. SqrtOfNegativeNumber"
  putStrLn "  6. ZeroRoot"
  resType <- getLine
  let num = read resType :: Int
  if num < 1 || num > 6
  then do
    putStrLn "You should input the number from 1 to 6"
    getResType
  else
    return num

main :: IO ()
main = do
  resType <- getResType
  if resType == 1
  then do
    putStrLn "Enter the value"
    answer <- getLine
    let value = read answer :: Double
    putStrLn "Please, enter the number of expressions"
    amount <- getLine
    let num = read amount :: Int
    getNum value num
  else do
    putStrLn "Please, enter the number of expressions"
    amount <- getLine
    let num = read amount :: Int
    getError resType num
