module Main (main) where

import Expr

askErrorOrNumber :: IO Bool -- True - Error; False - Number
askErrorOrNumber = do
  putStrLn "\nHello!\nEnter Error, if you want me to generate error. Enter Number otherwise."
  ans <- getLine
  if ans == "Error"
  then
    return True  
  else
    return False

readErrorType :: IO ArithmeticError
readErrorType = do
  putStrLn "Enter error type"
  errorType <- getLine
  case errorType of
    "Division by zero" -> return DivisionByZero
    "Log of zero" -> return LogOfZero
    "Log of negative number" -> return LogOfNegativeNumber
    _ -> return SqrtOfNegativeNumber

readNumber :: IO Double
readNumber = do
  putStrLn "Enter number"
  num <- getLine
  let parsedNum = read num :: Double
  return parsedNum 

readCount :: IO Int
readCount = do
  putStrLn "Enter number of expression"
  num <- getLine
  let parsedNum = read num :: Int
  return parsedNum

main :: IO ()
main = do
  err <- askErrorOrNumber
  result <- if err 
  then do
    errType <- readErrorType
    return $ Left errType
  else do
    number <- readNumber
    return $ Right number
  exprNumber <- readCount
  mapM_ print $ take exprNumber $ cycle $ generateExprByResult result