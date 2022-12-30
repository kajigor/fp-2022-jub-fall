module Expr where

data Expr = BinOp Op Expr Expr
          | Number Double
          | X
          deriving (Show, Eq)

data Op = Plus
        | Minus
        | Mult
        | Div
        deriving (Show, Eq)

eval :: Expr -> (Double -> Double)
eval (Number a) = const a
eval (BinOp op a b) =
    \x -> fromOp op (eval a x) (eval b x)
  where
    fromOp :: Op -> (Double -> Double -> Double)
    fromOp Plus = (+)
    fromOp Minus = (-)
    fromOp Mult = (*)
    fromOp Div = (/)
eval X = id