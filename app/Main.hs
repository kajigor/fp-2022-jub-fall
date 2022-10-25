module Main (main) where

import Expr
import GHC.Base (VecElem(Int16ElemRep))

generateTests :: Either ArithmeticError Double -> Int -> IO ()
generateTests res amount = do
   mapM_ print (take amount $ generateExprByResult res)

-- Лаконичный main.
main :: IO ()
main = do
  putStrLn "\nHi! Please input resut number or name of error you would like"
  result <- getLine
  putStrLn "\nPlease input amount of expressions you want"
  amount' <- getLine
  let amount = read amount' :: Int
  
  case result of 
    "DivisionByZero" -> generateTests (Left DivisionByZero) amount
    "LogOfZero" -> generateTests (Left LogOfZero) amount
    "LogOfNegativeNumber" -> generateTests (Left LogOfNegativeNumber) amount
    "RootOfIncorrectDegree" -> generateTests (Left RootOfIncorrectDegree) amount
    "EvenDegreeRootOfNegativeNumber" -> generateTests (Left EvenDegreeRootOfNegativeNumber) amount
    otherwise -> generateTests (Right (read result :: Double)) amount

