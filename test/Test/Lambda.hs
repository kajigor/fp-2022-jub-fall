module Test.Lambda where

import Lambda
import Test.Tasty.HUnit

unit_show :: IO ()
unit_show = do
   show Lambda.true @?= "\\x.\\y.x"
   show Lambda.successor @?= "\\n.\\f.\\x.f(n f x) "
   show Lambda.ifThenElse @?= "\\p.\\a.\\b.p a b"
   show Lambda.and @?= "\\p.\\q.p q p"

unit_show1 :: IO ()
unit_show1 = do
   show (toDeBruijn Lambda.true) @?= "\\.\\.0"
   show (toDeBruijn Lambda.successor) @?= "\\.\\.\\.1(0 1 2) "
   show (toDeBruijn Lambda.ifThenElse) @?= "\\.\\.\\.0 1 2"
   show (toDeBruijn Lambda.and) @?= "\\.\\.0 1 0"

unit_show3 = do
    assertBool "err" (alphaEq (Var "x") (Var "y"))
    assertBool "err" (alphaEq (Abs "x" (Var "x")) (Abs "y" (Var "y")))
    assertBool "err" (alphaEq (Abs "x" (App (Var "x") (Var "x"))) (Abs "y" (App (Var "y") (Var "y"))))