module Test.Lambda where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Lambda

lambda1 :: Lambda Int
lambda1 = (Var 6)

lambda2 :: Lambda Int
lambda2 = (App lambda1 (Var (89)))

lambda3 :: Lambda Int
lambda3 = (Abs (-4) lambda1)

unit_show = do
  show true @?= "\\x.\\y.x"
  show false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show Lambda.not @?= "\\p.p (\\x.\\y.y) (\\x.\\y.x)"
  show ifThenElse @?= "\\p.\\a.\\b.p a b"
  show one @?= "\\f.\\x.f x"
  show two @?= "\\f.\\x.f (f x)"
  show lambda1 @?= "6"
  show lambda2 @?= "6 89"
  show lambda3 @?= "\\-4.6"
  show (Var "x") @?= "x"
  show (Abs "q" (Var "p")) @?= "\\q.p"
