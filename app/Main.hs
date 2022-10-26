module Main (main) where
import Expr ( ArithmeticError (..), Expr (..), 
  generateExprByResult, 
  isArithmeticErrorExpected,
  convertStrToArithmeticError )


convertToResult :: String -> Either ArithmeticError Double
convertToResult str = if isArithmeticErrorExpected str 
  then
    Left (convertStrToArithmeticError str)
  else
    Right (read str :: Double)

printExprs :: [Expr] -> IO()
printExprs [] = putStr ""
printExprs (x:xs) = do
  print x
  printExprs xs

main :: IO ()
main = do
  putStrLn "What is the expected result for generated expressions?"
  resultStr <- getLine
  let result = convertToResult resultStr
  putStrLn "How many expressions should be generated?"
  numStr <- getLine
  let num = read numStr :: Int
  printExprs (generateExprByResult result num)