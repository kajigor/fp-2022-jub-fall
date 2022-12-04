{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Lib
import Parser
import Expr
import Data.Set as Set
import Data.Map as Map
import Data.Char
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe as MaybeT


getExpr :: MaybeT IO Expr
getExpr = do
    lift $ putStrLn "Enter function. Function should consist of +/-/*/ '/' /^."
    lift $ putStrLn "There can also be unary operators: sin/cos/abs."
    lift $ putStrLn "Function can contain few variables but only one of them "
    lift $ putStrLn "can be used for integration (values of other you should enter later)."
    s <- lift getLine
    case runParser exprParser s of
        Just ("", expr) -> return expr
        _ -> mzero


getExprRepeated :: MaybeT IO Expr
getExprRepeated = do
    msum $ repeat getExpr


isCorrectVar :: String -> Bool
isCorrectVar = all isLower

getVar :: MaybeT IO String
getVar = do
    lift $ putStrLn "Enter integration variable."
    s <- lift getLine
    guard (isCorrectVar s)
    return s

getVarRepeated :: MaybeT IO String
getVarRepeated = do
    msum $ repeat getVar


isCorrectDouble :: String -> Bool
isCorrectDouble s = case runParser number s of
    Just ("", _) -> True
    _ -> False

getDouble :: MaybeT IO String
getDouble = do
    s <- lift getLine
    guard (isCorrectDouble s)
    return s

getLeftRepeated :: MaybeT IO String
getLeftRepeated = do
    lift $ putStrLn "Enter left bound"
    msum $ repeat getDouble

getRightRepeated :: MaybeT IO String
getRightRepeated = do
    lift $ putStrLn "Enter right bound"
    msum $ repeat getDouble


getValueRepeated :: Show a => a -> MaybeT IO String
getValueRepeated s = do
    lift $ putStr "Enter value for parameter "
    lift $ print s
    msum $ repeat getDouble

getError :: MaybeT IO String
getError = do
    lift $ putStrLn "Enter positive error"
    s <- lift getLine
    guard (isCorrectDouble s)
    guard ((read s :: Double) > 0)
    return s

getErrorRepeated :: MaybeT IO String
getErrorRepeated = do
    msum $ repeat getError


askValue :: String -> IO Double
askValue s = do
    val <- runMaybeT $ getValueRepeated s
    return $ case val of ~(Just v) -> read v :: Double


parseMethod :: String -> Maybe Method 
parseMethod s = case s of
    "linear" -> Just LinearApproximation
    "rectangle" -> Just MiddleRectange
    "parabolic" -> Just ParabolicApproximation
    _ -> Nothing

isCorrectMethod :: String -> Bool
isCorrectMethod s = case parseMethod s of
    Just _ -> True
    _ -> False

getMethod :: MaybeT IO String
getMethod = do
    lift $ putStrLn "Enter method (linear/rectangle/parabolic)"
    s <- lift getLine
    guard (isCorrectMethod s)
    return s

getMethodRepeated :: MaybeT IO String
getMethodRepeated = do
    msum $ repeat getMethod

main :: IO ()
main = do
    putStrLn "Hello! It is integration console application!\n\n"

    Just expr <- runMaybeT getExprRepeated
    Just var <- runMaybeT getVarRepeated
    Just str_left <- runMaybeT getLeftRepeated
    Just str_right <- runMaybeT getRightRepeated

    let vars = Set.delete var $ variables expr

    values <- mapM askValue (Set.toList vars)

    let new_expr = setValues expr (Map.fromList $ zip (Set.toList vars) values)

    Just str_error <- runMaybeT getErrorRepeated

    Just (Just method) <- runMaybeT (parseMethod <$> getMethodRepeated)

    putStr "integral value is: "
    print $ integrateWithError 
        method 
        (eval new_expr var) 
        (read str_left :: Double) 
        (read str_right :: Double) 
        (read str_error :: Double) 