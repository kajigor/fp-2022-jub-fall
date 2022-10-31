module Main (main) where

import Expr

data Command = GenerateError | DisplayExpression

getCommand :: IO Command
getCommand = do
  putStrLn "Please choose what you want to do:\n1)generate an error\n2)display expression"
  answer <- getLine
  if answer == "1"
    then
      return GenerateError
    else if answer == "2"
        then
          return DisplayExpression
        else do
            putStrLn "Your input isn't correct."
            ans <- getCommand
            return ans


getError :: IO ArithmeticError
getError = do
  putStrLn "Please Enter your error:\n1)Division by zero error\n2)Log of zero error\n3)Log of negative number error\n4)Negative number Sqrt error"
  input <- getLine
  case input of
    "1" -> return DivisionByZero
    "2" -> return LogOfZero
    "3" -> return LogOfNegativeNumber
    "4" -> return NegativeNumberSqrt
    _ -> do
          putStrLn "Your input isn't correct."
          ans <- getError
          return ans


getNumber :: IO Double
getNumber = do
  putStrLn "Please enter your number"
  number <- getLine
  let num = read number :: Double
  return num 

getQuantity :: IO Int
getQuantity = do
  putStrLn "Please type number of expressions"
  number <- getLine
  let num = read number :: Int
  return num 

main :: IO ()
main = do
  command <- getCommand
  answer <- case command of
    GenerateError -> do
        error_ <- getError
        return $ Left error_
    DisplayExpression -> do
        number <- getNumber
        return $ Right number
  exprNumber <- getQuantity
  mapM_ print (take exprNumber (generateExprByResult answer))