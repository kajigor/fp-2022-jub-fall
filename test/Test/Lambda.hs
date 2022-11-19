module Test.Lambda where

import Lambda
import Test.Tasty.HUnit

import Text.Printf

unit_show_lambda :: IO ()
unit_show_lambda = do
  show Lambda.true @?= "\\x.\\y.x"
  show Lambda.false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show Lambda.not @?= "\\p.p ("++ show Lambda.false ++ ") ("++ show Lambda.true ++ ")"
  show Lambda.ifThenElse @?= "\\p.\\a.\\b.p a b"
  show Lambda.zero @?= "\\f.\\x.x"
  show Lambda.one @?= "\\f.\\x.f x"
  show Lambda.three @?= "\\f.\\x.f (f (f x))"
  show Lambda.add @?= "\\m.\\n.\\f.\\x.m f (n f x)"
  show Lambda.successor @?= "\\n.\\f.\\x.f (n f x)"
  show Lambda.mult @?= "\\m.\\n.\\f.m (n f)"
  show Lambda.t @?="v0"

unit_show_debruijn :: IO ()
unit_show_debruijn = do
  show (toDeBruijn Lambda.true) @?= "\\ \\ 1"
  show (toDeBruijn Lambda.false) @?= "\\ \\ 0"
  show (toDeBruijn Lambda.and) @?= "\\ \\ 1 0 1"
  show (toDeBruijn Lambda.or) @?= "\\ \\ 1 1 0"
  show (toDeBruijn Lambda.not) @?=  "\\ 0 (\\ \\ 0) (\\ \\ 1)"
  show (toDeBruijn Lambda.ifThenElse) @?= "\\ \\ \\ 2 1 0"
  show (toDeBruijn Lambda.zero) @?= "\\ \\ 0"
  show (toDeBruijn Lambda.one) @?= "\\ \\ 1 0"
  show (toDeBruijn Lambda.three) @?= "\\ \\ 1 (1 (1 0))"
  show (toDeBruijn Lambda.add) @?= "\\ \\ \\ \\ 3 1 (2 1 0)"
  show (toDeBruijn Lambda.successor) @?= "\\ \\ \\ 1 (2 1 0)"
  show (toDeBruijn Lambda.mult) @?= "\\ \\ \\ 2 (1 0)"
  