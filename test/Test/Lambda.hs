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
  assertBool "equal" (alphaEq Lambda.true Lambda.true)
  assertBool "equal" (alphaEq Lambda.false Lambda.zero)
  assertBool "not equal" (Prelude.not (alphaEq Lambda.true Lambda.false))
  assertBool "not equal" (Prelude.not (alphaEq Lambda.and Lambda.or))
  assertBool "not equal" (Prelude.not (alphaEq Lambda.true Lambda.successor))