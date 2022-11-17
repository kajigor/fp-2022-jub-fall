module Expr.AST where

import Text.Printf (printf)

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

printOp :: Op -> String
printOp Plus = "+"
printOp Minus = "-"
printOp Mult = "*"
printOp Div = "/"
printOp Pow = "^"

printInfix :: Expr -> String
printInfix (BinOp op l r) = printf "(%s %s %s)" (printInfix l) (printOp op) (printInfix r)
printInfix (Number n) = show n

printPrefix :: Expr -> String
printPrefix (BinOp op l r) = printf "%s %s %s" (printOp op) (printPrefix l) (printPrefix r)
printPrefix (Number n) = show n

-- parser (printer expr) == expr
