module Main (main) where
import Expr

getAmountOfExpr :: IO Int
getAmountOfExpr = do
  putStr "Please, enter the amount of expressions you want to see: "
  answer <- getLine
  let amount = read answer :: Int
  if amount <= 0
    then do
      putStrLn "The amount must be a positive integer."
      getAmountOfExpr
  else return amount

getCommand :: IO ()
getCommand = do
  putStrLn "Choose a type of an expression you want to see:"
  putStrLn "1) expression with an error"
  putStrLn "2) expression with an number"
  answer <- getLine
  case answer of
    "1" -> getError
    "2" -> getNumber
    _ -> do
      putStrLn "Your answer should be 1 or 2."
      getCommand

getError :: IO ()
getError = do
  putStrLn "Please, choose which error you would like to see:"
  putStrLn "1) Divison by zero"
  putStrLn "2) Logarithm of zero"
  putStrLn "3) Logarithm of a negative number"
  putStrLn "4) Root of a negative number"
  putStrLn "5) Root with a zero degree"

  answer <- getLine
  case answer of 
    "1" -> print $ generateExprByResult $ Left DivisionByZero
    "2" -> print $ generateExprByResult $ Left LogOfZero
    "3" -> print $ generateExprByResult $ Left LogOfNegativeNumber
    "4" -> print $ generateExprByResult $ Left RootNegativeNumber
    "5" -> print $ generateExprByResult $ Left RootZeroDegree
    _ -> do
      putStrLn "Your answer should be between 1 and 5."
      getError

getNumber :: IO ()
getNumber = do
  putStr "Please, enter a number: "
  answer <- getLine
  let number = read answer :: Double
  print $ generateExprByResult $ Right number

run_getCommand :: Int -> IO ()
run_getCommand 0 = return ()
run_getCommand n = do
  getCommand
  run_getCommand (n-1)

main :: IO ()
main = do
  amountOfExpr <- getAmountOfExpr
  run_getCommand amountOfExpr

