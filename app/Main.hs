module Main (main) where
import Expr

askExpr :: IO (Either ArithmeticError Double, Int)
askExpr = do
  putStrLn "\nWhat result of the expression do you want?"
  result <- getLine
  putStrLn "\nHow many expressions do you want?"
  number <- getLine
  return (read result, read number)

-- Лаконичный main.
main :: IO ()
main = do
  (result, number) <- askExpr
  mapM_ print $ generateExprByResultAndNumber result number
  