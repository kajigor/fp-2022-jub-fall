module Main (main) where
import Expr
import Data.Maybe
import Text.Read

askExpr :: IO (Either ArithmeticError Double, Int)
askExpr = do
  putStrLn "\nWhat result of the expression do you want?"
  result <- getLine
  putStrLn "\nHow many expressions do you want?"
  number <- getLine
  return (read (parseResult result), read number)
  where
    parseResult :: String -> String
    parseResult result | isNothing (readMaybe result :: Maybe Double) = "Left " ++ result
                       | otherwise = "Right " ++ result

-- Лаконичный main.
main :: IO ()
main = do
  (result, number) <- askExpr
  mapM_ print $ generateExprByResultAndNumber result number
  