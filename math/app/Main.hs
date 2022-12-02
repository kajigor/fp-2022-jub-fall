module Main (main) where

import Lib
import Parser
import Expr
import Data.Set as Set
import Data.Map as Map
import Data.Char
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe as MaybeT

isCorrectExpr :: String -> Bool
isCorrectExpr s =
    case runParser exprParser s of
        Just ("", expr) -> True
        _ -> False

getExpr :: MaybeT IO Expr
getExpr = do
    s <- lift getLine
    guard (isCorrectExpr s)
    case runParser exprParser s of
        ~(Just ("", expr)) -> return expr

getExprRepeated = do
    lift $ putStrLn "Enter function"
    msum $ repeat getExpr


isCorrectVar :: String -> Bool
isCorrectVar = all isLower

getVar :: MaybeT IO String
getVar = do
    s <- lift getLine
    guard (isCorrectVar s)
    return s

getVarRepeated = do
    lift $ putStrLn "Enter integration variable"
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

getLeftRepeated = do
    lift $ putStrLn "Enter left bound"
    msum $ repeat getDouble

getRightRepeated = do
    lift $ putStrLn "Enter right bound"
    msum $ repeat getDouble


getValueRepeated s = do
    lift $ putStr "Enter value for parameter "
    lift $ print s
    msum $ repeat getDouble

getError = do
    s <- lift getLine
    guard (isCorrectDouble s)
    guard ((read s :: Double) > 0)
    return s

getErrorRepeated = do
    lift $ putStrLn "Enter positive error"
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
    s <- lift getLine
    guard (isCorrectMethod s)
    return s

getMethodRepeated = do
    lift $ putStrLn "Enter method (linear/rectangle/parabolic)"
    msum $ repeat getMethod

main = do
    mbexpr <- runMaybeT getExprRepeated
    let expr = case mbexpr of ~(Just e) -> e
    mbvar <- runMaybeT getVarRepeated
    let var = case mbvar of ~(Just v) -> v
    mbleft <- runMaybeT getLeftRepeated
    let left = case mbleft of ~(Just l) -> read l :: Double
    mbright <- runMaybeT getRightRepeated
    let right = case mbright of ~(Just r) -> read r :: Double

    let vars = Set.delete var $ variables expr

    values <- mapM askValue (Set.toList vars)

    let new_expr = setValues expr (Map.fromList $ zip (Set.toList vars) values)

    mberror <- runMaybeT getErrorRepeated
    let error = case mberror of ~(Just e) -> read e :: Double

    mbmethod <- runMaybeT getMethodRepeated
    let smethod = case mbmethod of ~(Just s) -> s
    let method = case parseMethod smethod of ~(Just m) -> m

    putStr "integral value is: "
    print $ integrateWithError ParabolicApproximation (eval new_expr var) left right error