module Main (main) where
import Expr

introduction :: IO (Result, Int)
introduction = do
  putStrLn "Enter the result (e.g. 'Right 42' or 'Left LogOfZero'):"
  result <- getLine
  putStrLn "Enter how many expressions to find:"
  count <- getLine
  return (read result, read count)  

main :: IO ()
main = do
  (result, count) <- introduction
  mapM_ print $ take count $ generateExprByResult result