module Test.Lambda where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Lambda

lambda_pos :: Lambda Int
lambda_pos = (App (Var (2)) (Var (1)))

lambda_neg :: Lambda Int
lambda_neg = (Abs (-1) (Var (2)))

lambda1 :: DeBruijn 
lambda1 = AbsDB (AbsDB (VarDB 1))



unit_show = do
  show true @?= "\\x.\\y.x"
  show false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show Lambda.not @?= "\\p.p (\\x.\\y.y) (\\x.\\y.x)"
  show one @?= "\\f.\\x.f x"
  show two @?= "\\f.\\x.f (f x)"
  show three @?= "\\f.\\x.f (f (f x))"
  show lambda_pos @?= "2 1"
  show lambda_neg @?= "\\-1.2"


unit_db = do
    show lambda1 @?= "\\.\\.1"
    

