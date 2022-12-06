module Expr.AST where

data Expr = BinOp Op Expr Expr
          | Number Int
          | Boolean Bool
          | If Expr Expr Expr
          deriving (Show, Eq)

data Op = Plus
        | Minus
        | Mult
        | Div
        | Pow
        | Eq
        | Neq
        | Lt
        | Le
        | Gt
        | Ge
        | And
        | Or
        deriving (Show, Eq)

eval :: Expr -> Either String Int
eval (Number x) = return x
eval (Boolean b) = return $ toInt b
eval (If c t e) = do
  c <- eval c
  if fromInt c
  then eval t
  else eval e
eval (BinOp op x y) = do
    x <- eval x
    y <- eval y
    return $ fromOp op x y
  where
    fromOp Plus = (+)
    fromOp Minus = (-)
    fromOp Mult = (*)
    fromOp Div = div
    fromOp Pow = (^)
    fromOp Eq = \x y -> toInt $ x == y
    fromOp Neq = \x y -> toInt $ x /= y
    fromOp Le = \x y -> toInt $ x <= y
    fromOp Lt = \x y -> toInt $ x < y
    fromOp Ge = \x y -> toInt $ x >= y
    fromOp Gt = \x y -> toInt $ x > y
    fromOp And = \x y -> toInt $ fromInt x && fromInt y
    fromOp Or = \x y -> toInt $ fromInt x || fromInt y

toInt :: Bool -> Int
toInt True = 0
toInt False = 1

fromInt :: Int -> Bool
fromInt 0 = True
fromInt _ = False

printOp :: Op -> String
printOp Plus = "+"
printOp Minus = "-"
printOp Mult = "*"
printOp Div = "/"
printOp Pow = "^"
printOp Eq = "=="
printOp Neq = "/="
printOp Lt = "<"
printOp Le = "<="
printOp Gt = ">"
printOp Ge = ">="
printOp And = "&&"
printOp Or = "||"
