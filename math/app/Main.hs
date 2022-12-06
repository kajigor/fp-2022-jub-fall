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
import Data.Maybe


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


getDouble :: MaybeT IO Double
getDouble = do
    s <- lift getLine
    case runParser number s of
        (Just ("", a)) -> return a
        _ -> mzero

getLeftRepeated :: MaybeT IO Double
getLeftRepeated = do
    lift $ putStrLn "Enter left bound"
    msum $ repeat getDouble

getRightRepeated :: MaybeT IO Double
getRightRepeated = do
    lift $ putStrLn "Enter right bound"
    msum $ repeat getDouble


getValueRepeated :: Show a => a -> MaybeT IO Double
getValueRepeated s = do
    lift $ putStr "Enter value for parameter "
    lift $ print s
    msum $ repeat getDouble

getError :: MaybeT IO Double
getError = do
    lift $ putStrLn "Enter positive error"
    s <- lift getLine
    case runParser number s of
        (Just ("", a)) -> if a > 0 
                          then return a
                          else mzero
        _ -> mzero

getErrorRepeated :: MaybeT IO Double
getErrorRepeated = do
    msum $ repeat getError

parseMethod :: String -> Maybe Method 
parseMethod s = case s of
    "linear" -> Just LinearApproximation
    "rectangle" -> Just MiddleRectange
    "parabolic" -> Just ParabolicApproximation
    _ -> Nothing

getMethod :: MaybeT IO Method
getMethod = do
    lift $ putStrLn "Enter method (linear/rectangle/parabolic)"
    s <- lift getLine
    case parseMethod s of
        (Just method) -> return method
        _ -> mzero

getMethodRepeated :: MaybeT IO Method
getMethodRepeated = do
    msum $ repeat getMethod

main :: IO ()
main = do
    putStrLn "Hello! It is integration console application!\n\n"

    Just expr <- runMaybeT getExprRepeated
    Just var <- runMaybeT getVarRepeated
    Just left <- runMaybeT getLeftRepeated
    Just right <- runMaybeT getRightRepeated

    let vars = Set.delete var $ variables expr

    values <- mapM runMaybeT $ fmap getValueRepeated (Set.toList vars)

    let upd_values = fmap fromJust values

    let new_expr = setValues expr (Map.fromList $ zip (Set.toList vars) upd_values)

    Just error' <- runMaybeT getErrorRepeated

    Just method <- runMaybeT getMethodRepeated

    putStr "integral value is: "
    print $ integrateWithError 
        method 
        (eval new_expr var) 
        left
        right
        error'