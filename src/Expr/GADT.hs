{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Expr.GADT where

-- Generalized Algebraic Data Types

data Expr a where
  Number :: Int -> Expr Int
  Boolean :: Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  BinOpInt :: OpInt -> Expr Int -> Expr Int -> Expr Int
  BinOpCmp :: Ord a => OpCmp -> Expr a -> Expr a -> Expr Bool
  BinOpBool :: OpBool -> Expr Bool -> Expr Bool -> Expr Bool

eval :: Expr a -> a
eval (Number x) = x
eval (Boolean b) = b
eval (If c t e) =
  let c' = eval c in
  if c'
  then eval t
  else eval e
eval (BinOpInt op l r) =
  let l' = eval l in
  let r' = eval r in
  fromIntOp op l' r'
eval (BinOpCmp op x y) =
  let x' = eval x in
  let y' = eval y in
  fromCmpOp op x' y'
eval (BinOpBool op x y) =
  let x' = eval x in
  let y' = eval y in
  fromBoolOp op x' y'


data OpInt
  = Plus
  | Minus
  | Mult
  | Div
  | Pow
  deriving (Show, Eq)

data OpBool
  = And
  | Or
  deriving (Show, Eq)

data OpCmp
  = Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Show, Eq)


fromIntOp :: Integral a => OpInt -> a -> a -> a
fromIntOp Plus = (+)
fromIntOp Minus = (-)
fromIntOp Mult = (*)
fromIntOp Div = div
fromIntOp Pow = (^)

fromCmpOp :: Ord a => OpCmp -> a -> a -> Bool
fromCmpOp Eq = (==)
fromCmpOp Neq = (/=)
fromCmpOp Le = (<=)
fromCmpOp Lt = (<)
fromCmpOp Ge = (>=)
fromCmpOp Gt = (>)

fromBoolOp :: OpBool -> Bool -> Bool -> Bool
fromBoolOp And = (&&)
fromBoolOp Or = (||)

