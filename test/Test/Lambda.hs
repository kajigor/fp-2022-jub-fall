module Test.Lambda where

import Lambda
import Test.Tasty.HUnit

unit_show = do
  show Lambda.true @?= "\\x.\\y.x"
  show Lambda.zero @?= "\\f.\\x.x"
  show Lambda.one @?= "\\f.\\x.f x"
  show Lambda.two @?= "\\f.\\x.f (f x)"
  show Lambda.three @?= "\\f.\\x.f (f (f x))"
  show Lambda.four @?= "\\f.\\x.f (f (f (f x)))"

unit_toDeBruijn = do
  show (toDeBruijn Lambda.true) @?= "\\\\2"
  show (toDeBruijn Lambda.and) @?= "\\\\2 1 2"
  show (toDeBruijn Lambda.ifThenElse) @?= "\\\\\\3 2 1"
  show (toDeBruijn Lambda.two) @?= "\\\\2 (2 1)"
  show (toDeBruijn Lambda.three) @?= "\\\\2 (2 (2 1))"

unit_alphaEq = do
  alphaEq Lambda.true Lambda.true @?= True
  alphaEq Lambda.false Lambda.zero @?= True
  alphaEq Lambda.true Lambda.false @?= False
  alphaEq Lambda.and Lambda.or @?= False
  alphaEq Lambda.true Lambda.successor @?= False