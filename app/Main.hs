module Main (main) where
import Expr




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
