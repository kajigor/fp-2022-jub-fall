{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Parser where

import Test.Tasty.HUnit (Assertion, (@?=))

import Expr
import Parser(parse)

unit_parser :: Assertion
unit_parser = do
    parse "1+2*3+(45-1)/7" @?= Just (BinOp Plus (BinOp Plus (Number 1.0) (BinOp Mult (Number 2.0) (Number 3.0))) (BinOp Div (BinOp Minus (Number 45.0) (Number 1.0)) (Number 7.0)))
    parse "x*x/x+3*x-1/x" @?= Just (BinOp Minus (BinOp Plus (BinOp Div (BinOp Mult X X) X) (BinOp Mult (Number 3.0) X)) (BinOp Div (Number 1.0) X))
    parse "3x" @?= Nothing
    parse "+x" @?= Nothing
