module Expr where

import Data.Set as Set
import Data.Map as Map

data Expr = BinOp BinOp Expr Expr
          | Number Double
          | Var String
          | UnaryOp UnaryOp Expr
          deriving (Show, Eq)

data BinOp = Plus
           | Minus
           | Mult
           | Div
           | Pow
           deriving (Show, Eq)

data UnaryOp = UnaryMinus
             | Sin
             | Cos
             | Abs
             deriving (Show, Eq)


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
setValues (Number x) map = Number x
setValues (BinOp op a b) map = BinOp op (setValues a map) (setValues b map)
setValues (UnaryOp op a) map = UnaryOp op (setValues a map)
setValues (Var x) map = case Map.lookup x map of
    Nothing -> Var x
    Just num -> Number num