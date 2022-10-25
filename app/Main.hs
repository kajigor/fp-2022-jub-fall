module Main (main) where

import Expr

main :: IO ()
main = do
  putStrLn "Expected value: "
  val <- getLine
  putStrLn "Number of expressions: "
  n <- getLine
  let parsedN = read n :: Int
  let parsedValue = parseValue val
  print (take parsedN $ generateExprByResult parsedValue)

parseValue:: String -> Either ArithmeticError Double
parseValue "DivisionByZero" = Left DivisionByZero
parseValue "LogOfZero" = Left LogOfZero
parseValue "LogOfNegativeNumber" = Left LogOfNegativeNumber
parseValue "EvenRootOfNegativeNumber" = Left EvenRootOfNegativeNumber
parseValue val = Right (read val :: Double)
