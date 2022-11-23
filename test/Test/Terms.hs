module Test.Terms where

import Lambda

t1 = Var "x"

t2 = App (Var "x") (Var "y")

t3 = App (Var "x") (Var "x")

t4 = Abs "a" (Var "x")

t5 = App (Abs "a" (Var "x")) (Abs "b" (Var "x"))

t6 = App (Abs "a" (Var "x")) (Abs "b" (Var "y"))

t7 = App (Abs "a" (Var "a")) (Abs "b" (Var "a"))

t8 = Abs "a" (App (Var "x") (Var "y"))

t9 = App (Abs "a" (Var "a")) (Abs "b" (Var "x"))

t10 = App (Abs "a" (Var "a")) (Abs "b" (Var "c"))

t11 = Abs "x" (Var "a")

t12 = Abs "y" (Var "x")

t13 = Abs "y" (Abs "a" (Var "x"))

tId = Abs "x" (Var "x")

tS = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

tDouble = Abs "x" (App (Var "x") (Var "x"))

tAddOne = App Lambda.add Lambda.one

tAddOneOne = App tAddOne Lambda.one

tAddOneTwo = App tAddOne Lambda.two

t14 = App (Var "y") (App (Abs "x" (Var "x")) (Var "z"))

t15 = App (Abs "x" t1) (Abs "x" t5)


