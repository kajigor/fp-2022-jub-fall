module Main (main) where
import Expr

introduction :: IO ()
introduction = do
  putStrLn "\nHi! I'll generate expressions for you"

askResult :: IO (Either ArithmeticError Double)
askResult = do
  putStrLn "\nWhat results do you like?"
  res <- getLine
  let parsedRes = read res :: Either ArithmeticError Double
  return parsedRes

-- Спрашиваем у пользователя его любимое число, парсим его, используя стандартный read.
-- Эта функция возвращает полученного число как результат, который мы дальше, в main, сможешь получить и использовать.
-- Вывод и ввод данных тут является эффектом, число -- результатом, что отражается в типе функции :: IO Int
askNumber :: IO Int
askNumber = do
  putStrLn "How many expressions should I generate?"
  num <- getLine
  let parsedNum = read num :: Int
  return parsedNum

-- Функции с IO -- полноценные функции.
-- У них могут быть не только возвращаемые значения, но и аргументы.
-- Например, тут мы реагируем на любимое число пользователя.
-- Функция принимает любимое число программы, и дальше использует его в своей реакции.
showExpressions :: Either ArithmeticError Double -> Int -> IO ()
showExpressions res n = do
    putStrLn $ show $ take n (generateExprByResult res)

-- Лаконичный main.
main :: IO ()
main = do
  introduction
  res <- askResult
  n <- askNumber
  showExpressions res n


-- Эта функция просто демонстрирует, во что дешугарится do-нотация
f :: IO ()
f = do
  x <- getLine        -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine        -- getLine >>= \y ->
  putStrLn (y ++ y)   -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2)  -- print (parsed * 2) >>= \r ->
                      -- return r
