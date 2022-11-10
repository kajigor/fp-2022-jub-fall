{-# LANGUAGE InstanceSigs #-}
module Test.Lambda where

import Lambda (true, false, and, zero, one, two, Lambda(..))
import Test.HUnit

unit_show_string :: Assertion
unit_show_string = do
    show (Var "s5") @?= "s5"
    show Lambda.true @?= "λx.λy.x"
    show Lambda.false @?= "λx.λy.y"
    show Lambda.and @?= "λp.λq.p q p"
    show Lambda.zero @?= "λf.λx.x"
    show Lambda.one @?= "λf.λx.f x"
    show Lambda.two @?= "λf.λx.f (f x)"


expr1 :: Lambda Int
expr1 = Var 1
expr2 :: Lambda Int
expr2 = Abs 2 (Abs 179 (Abs 1 (Abs 10 (App (App (Var 2) (Var 1)) (App (App (Var 179) (Var 1)) (Var 10))))))
unit_show_other :: Assertion
unit_show_other = do
    show expr1 @?= "1"
    show expr2 @?= "λ2.λ179.λ1.λ10.2 1 (179 1 10)"