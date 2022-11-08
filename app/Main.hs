module Main (main) where

import Expr
-- Лаконичный main.
main :: IO ()
main = do
  putStrLn "What value do you need?"
  x <- getLine
  let val = read x :: Either ArithmeticError Double
  putStrLn "How many expressions do you need?"
  y <- getLine
  let n = read y :: Int
  mapM_ print (take n (generateExprByResult val))