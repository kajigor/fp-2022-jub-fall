module Main (main) where

import Lib
import Parser
import Expr
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

aboba = do
    lift $ putStrLn "Enter expr"
    expr <- msum $ repeat getExpr
    lift $ print expr
    lift $ putStrLn "Enter expr"
    expr' <- msum $ repeat getExpr
    lift $ print expr'
    

main = do
    runMaybeT aboba

