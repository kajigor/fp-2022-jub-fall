module Main
  ( main
  ) where

import           Expr

-- Просто выводим строку на выход.
introduction :: IO ()
introduction = do
  putStrLn
    "\nHi! Input what kind of error you want to get\n0 — None\n1 — LogOfZero\n2 — LogOfNegativeNumber\n3 — NegativeRadicand\n4 — ZeroDegreeRoot"

-- Спрашиваем имя у пользователя и приветствуем его
parseErrorType :: IO Int
parseErrorType = do
  inp <- getLine
  let e_type = read inp :: Int
  return e_type

parseResult :: IO Double
parseResult = do
  putStrLn "\nInput result you want to get"
  inp <- getLine
  let res = read inp :: Double
  return res

parseNumTests :: IO Int
parseNumTests = do
  putStrLn "\nInput number of tests"
  inp <- getLine
  let res = read inp :: Int
  return res

errorFromId :: Int -> IO ArithmeticError
errorFromId id = do
  case id of
    1 -> return DivisionByZero
    2 -> return LogOfZero
    3 -> return LogOfNegativeNumber
    4 -> return NegativeRadicand
    5 -> return ZeroDegreeRoot
    _ -> undefined

generateTests :: Int -> Either ArithmeticError Double -> IO ()
generateTests n res = do
  mapM_ print (take n $ generateExprByResult res)

main :: IO ()
main = do
  introduction
  e_type <- parseErrorType
  if e_type == 0
    then do
      d_res <- parseResult
      n_tests <- parseNumTests
      generateTests n_tests (Right d_res)
    else do
      n_tests <- parseNumTests
      error <- errorFromId e_type
      generateTests n_tests (Left error)

-- Эта функция просто демонстрирует, во что дешугарится do-нотация
f :: IO ()
f = do
  x <- getLine -- getLine >>= \x ->
  let parsed = read x -- let parsed = read x in
  y <- getLine -- getLine >>= \y ->
  putStrLn (y ++ y) -- putStrLn (y ++ y) >>= \_ ->
  print (parsed * 2) -- print (parsed * 2) >>= \r ->
                      -- return r
