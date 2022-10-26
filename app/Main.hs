module Main (main) where

import Expr
import Data.List (intercalate)
import Text.Read (readEither)

askDesirableResult :: IO (Either String (Either ArithmeticError Double))
askDesirableResult = do
  putStrLn "What result do you want?(*Double*, DivisionByZero, LogOfZero, LogOfNegativeNumber, RootOfNegativeNumber, NegativeRoot)"
  input <- getLine
  let parsedRes = readEither input :: Either String Double
  case parsedRes of
    Right a -> return $Right (Right a)
    _ ->
      case input of
        "DivisionByZero" -> return $Right $ Left DivisionByZero
        "LogOfZero" -> return $Right $ Left LogOfZero
        "LogOfNegativeNumber" -> return $Right $ Left LogOfNegativeNumber
        "RootOfNegativeNumber" -> return $Right $ Left RootOfNegativeNumber
        "NegativeRoot" -> return $Right $ Left NegativeRoot
        _ -> return $ Left "Bad input"

askN :: IO Int
askN = do
  putStrLn "How many expressions do you want?"
  num <- getLine
  let parsedRes = read num :: Int
  return parsedRes


-- Лаконичный main.
main :: IO ()
main = do
  result <- askDesirableResult
  case result of 
    Right res -> do
      n <- askN
      putStrLn (intercalate "\n" $ map show (take n $ generateExprByResult res))
    Left message -> do
      putStrLn message