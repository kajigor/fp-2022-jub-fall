module Expr where

data Expr = BinOp Op Expr Expr
          | Number Int
          deriving (Show, Eq)

data Op = Plus
        | Minus
        | Mult
        | Div
        | Pow
        deriving (Show, Eq)

eval :: Expr -> Int
eval (Number x) = x
eval (BinOp op x y) =
    fromOp op (eval x) (eval y)
  where
    fromOp Plus = (+)
    fromOp Minus = \x y -> x - y
    fromOp Mult = (*)
    fromOp Div = div
    fromOp Pow = (^)