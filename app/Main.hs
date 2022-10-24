module Main (main) where
import Text.Read ( readEither )
import Expr (ArithmeticError, generateExprByResult, ArithmeticError(..))
import Data.List (intercalate)

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
  putStrLn "\nHi! Enter a result of expression. It could be a real number or one of errors: 'DivisionByZero', 'LogOfZero', 'LogOfNegativeNumber', 'EvenRootOfNegativeNumber'"

-- Я очень разобрался как тут аккуратно попарсить return case ... of не работает(
-- askForResult :: IO (Either String Int)
-- askForResult =  do
--   result <- getLine
--   let parsedInt = readEither result :: Either String Int
--   let parsedString = readEither result :: Either String String

enterNumberOfExpression :: IO Int
enterNumberOfExpression = do
  putStrLn "Enter amount of expressions to print: "
  line <- getLine
  let num = read line :: Int
  return num

printExprs :: Either ArithmeticError Double -> IO ()
printExprs x = do
  n <- enterNumberOfExpression
  let res = take n $ generateExprByResult x
  putStrLn (intercalate "\n" $ map show res)

-- Лаконичный main.
main :: IO ()
main = do
  introduction
  line <- getLine
  let parsedInt = readEither line :: Either String Double
  case parsedInt of
    Right a -> printExprs (Right a)
    _ ->
      case line of
        "DivisionByZero" -> printExprs $ Left DivisionByZero
        "LogOfZero" -> printExprs $ Left LogOfZero
        "LogOfNegativeNumber" -> printExprs $ Left LogOfNegativeNumber
        "EvenRootOfNegativeNumber" -> printExprs $ Left EvenRootOfNegativeNumber
        _ -> putStrLn "Unknown Error!"





-- Эта функция просто демонстрирует, во что дешугарится do-нотация
f :: IO ()
f = do
  x <- getLine        -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine        -- getLine >>= \y ->
  putStrLn (y ++ y)   -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2)  -- print (parsed * 2) >>= \r ->
                      -- return r
