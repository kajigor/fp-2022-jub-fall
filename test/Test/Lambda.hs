module Test.Lambda where

import Lambda
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

t1 = Abs "x" (Abs "y" (Abs "s" (Abs "z" (App (App (Var "x") (Var "s")) (App (App (Var "y") (Var "s")) (Var "z"))))))

t2 = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))

t3 = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

t4 = App (Abs "x" (Abs "x" (Var "x"))) (Abs "y" (Var "y"))

e1 = App (App (Var "x") (Var "y")) (Var "z")

e2 = App (App (Var "a") (Var "b")) (Var "c")

r1 = AbsDB (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))))

r2 = AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))

r3 = AbsDB (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))))

r4 = AppDB (AbsDB (AbsDB (VarDB 0))) (AbsDB (VarDB 0))

sub = Sub (Var "x") (Var "y")

e = Abs "x" (Abs "y" (App (Var "x") (Var "y")))

unit_lambdaShow = do
  show t1 @?= "λx.λy.λs.λz.x s(y s z)"
  show t2 @?= "λp.λa.λb.p a b"
  show t3 @?= "λm.λn.λf.λx.m f(n f x)"
  show t4 @?= "λx.λx.x λy.y"

--unit_deBruijn = do
--toDeBruijn t1 [] [] @?= r1
--toDeBruijn t2 [] @?= r2
--toDeBruijn t3 [] @?= r3
--toDeBruijn t4 [] @?= r4

unit_alphaEq = do
  alphaEq e1 e2 @?= True

unit_CAS = do
  show (cas e sub) @?= "λy.x y"
