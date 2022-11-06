module Main (main) where
import Expr


data Command = FromError | FromNumber

getCommand :: IO Command
getCommand = do
  putStrLn "Do you want me to generate a specific error or to generate an expression with a given answer? \n 1. Generate a specific error \n 2. Generate an expression"
  input <- getLine
  case input of
    "1" -> return FromError
    "2" -> return FromNumber
    _ -> do
      putStrLn "Invalid input\n"
      result <- getCommand
      return result


getError :: IO ArithmeticError
getError = do
  putStrLn "There are presented errors: \n 1. Log of zero \n 2. Log of negative number \n 3. Even root of negative number \n 4. Zero root \n 5. Division by zero"
  input <- getLine
  case input of
    "1" -> return LogOfZero
    "2" -> return LogOfNegativeNumber
    "3" -> return EvenRootOfNegativeNumber
    "4" -> return ZeroRoot
    "5" -> return DivisionByZero
    _ -> do
      putStrLn "Invalid input\n"
      result <- getError
      return result

getNumberOfExpr :: IO Int
getNumberOfExpr = do
  putStrLn "How many expressions do you want me to generate?"
  input <- getLine
  let num = read input :: Int
  if num < 0 then do
    putStrLn "Invalid input"
    result <- getNumberOfExpr
    return result
  else
    return num

getAnswer :: IO Double
getAnswer = do
  putStrLn "Which answer do you want to get?"
  input <- getLine
  let result = read input :: Double
  return result

main :: IO ()
main = do
  command <- getCommand
  expr_num <- getNumberOfExpr
  case command of
    FromError -> do
      wanted_error <- getError
      putStrLn $ show $ take expr_num $ generateExprByResult (Left wanted_error)
    FromNumber -> do
      answer <- getAnswer
      putStrLn $ show $ take expr_num $ generateExprByResult (Right answer)

