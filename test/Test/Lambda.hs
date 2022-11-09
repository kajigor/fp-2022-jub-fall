module Test.Lambda where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Lambda

weird_1 :: Lambda Int
weird_1 = (Var 1)

weird_2 :: Lambda Int
weird_2 = (Abs 1 weird_1)

weird_3 :: Lambda Int
weird_3 = (App weird_1 (Var (-1)))

unit_show = do
  show (Var "a") @?= "a"
  show (Abs "a" (Var "a")) @?= "\\a.a"
  show true @?= "\\x.\\y.x"
  show false @?= "\\x.\\y.y"
  show one @?= "\\f.\\x.f x"
  show two @?= "\\f.\\x.f (f x)"
  show weird_1 @?= "1"
  show weird_2 @?= "\\1.1"
  show weird_3 @?= "1 -1"

