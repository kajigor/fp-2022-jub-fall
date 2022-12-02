{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Expr where

import Data.Set as Set
import Data.Map as Map ( Map, lookup )
import Numeric

data Expr = BinOp BinOp Expr Expr
          | Number Double
          | Var String
          | UnaryOp UnaryOp Expr
          deriving Eq


instance Show Expr where 
    show :: Expr -> String
    show (BinOp op l r) = "(" ++ show l ++ ")" ++ show op ++ "(" ++ show r ++ ")"
    show (Number n) = showFFloat (Just 6) n ""
    show (Var x) = x
    show (UnaryOp uOp e) = show uOp ++ "(" ++ show e ++ ")"


data BinOp = Plus
           | Minus
           | Mult
           | Div
           | Pow
           deriving Eq


instance Show BinOp where
    show :: BinOp -> String
    show Plus = "+"
    show Minus = "-"
    show Div = "/"
    show Mult = "*"
    show Pow = "^"


data UnaryOp = UnaryMinus
             | Sin
             | Cos
             | Abs
             deriving Eq


instance Show UnaryOp where
    show UnaryMinus = "-"
    show Sin = "sin"
    show Cos = "cos"
    show Abs = "abs"


eval :: Expr -> String -> Double -> Double
eval (BinOp op l r) name val = operation op (eval l name val) (eval r name val)
    where
        operation :: BinOp -> Double -> Double -> Double
        operation Plus = (+)
        operation Minus = (-)
        operation Div = (/)
        operation Mult = (*)
        operation Pow = (**)
eval (Number n) _ _ = n
eval (Var y) name val | y == name = val
                      | otherwise = undefined
eval (UnaryOp op e) name val = operation op $ eval e name val
    where
        operation :: UnaryOp -> Double -> Double
        operation Sin = sin
        operation Cos = cos
        operation Abs = abs
        operation UnaryMinus = sin


variables :: Expr -> Set String
variables (Number _) = Set.empty
variables (BinOp _ a b) = variables a `Set.union` variables b
variables (Var x) = Set.singleton x
variables (UnaryOp _ a) = variables a


setValues :: Expr -> Map String Double -> Expr
setValues (Number x) _ = Number x
setValues (BinOp op a b) varsMap = BinOp op (setValues a varsMap) (setValues b varsMap)
setValues (UnaryOp op a) varsMap = UnaryOp op (setValues a varsMap)
setValues (Var x) varsMap = case Map.lookup x varsMap of
    Nothing -> Var x
    Just num -> Number num