module Test.Lambda where

import Lambda
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

t1 = Abs "x" (Abs "y" (Abs "s" (Abs "z" (App (App (Var "x") (Var "s")) (App (App (Var "y") (Var "s")) (Var "z"))))))

t2 = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))

t3 = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

t4 = App (Abs "x" (Abs "x" (Var "x"))) (Abs "y" (Var "y"))

t5 = App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (App (Var "x") (Var "x")))

t6 = App (Abs "x" (Abs "x" (Var "x"))) (Abs "y" (Var "y"))

e1 = App (App (Var "x") (Var "y")) (Var "z")

e2 = App (App (Var "a") (Var "b")) (Var "c")

e3 = Abs "a" (Abs "b" (Abs "c" (Abs "d" (App (App (Var "a") (Var "c")) (App (App (Var "b") (Var "c")) (Var "d"))))))

e4 = App (Abs "d" (Abs "d" (Var "d"))) (Abs "w" (Var "w"))

e5 = App (Abs "q" (App (Var "q") (Var "q"))) (Abs "g" (App (Var "g") (Var "g")))

r1 = AbsDB (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))))

r2 = AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))

r3 = AbsDB (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))))

r4 = AppDB (AbsDB (AbsDB (VarDB 0))) (AbsDB (VarDB 0))

r5 = AppDB (AbsDB (AppDB (VarDB 0) (VarDB 0))) (AbsDB (AppDB (VarDB 0) (VarDB 0)))

r6 = AppDB (AbsDB (AbsDB (VarDB 0))) (AbsDB (VarDB 0))

sub = Sub (Var "x") (Var "y")

e = Abs "x" (Abs "y" (App (Var "x") (Var "y")))

unit_lambdaShow = do
  show t1 @?= "λx.λy.λs.λz.x s(y s z)"
  show t2 @?= "λp.λa.λb.p a b"
  show t3 @?= "λm.λn.λf.λx.m f(n f x)"
  show t4 @?= "λx.λx.x λy.y"
  show t5 @?= "λx.x x λx.x x"
  show t6 @?= "λx.λx.x λy.y"

unit_deBruijn = do
  toDeBruijn t1 [] [] @?= r1
  toDeBruijn t2 [] [] @?= r2
  toDeBruijn t3 [] [] @?= r3
  toDeBruijn t4 [] [] @?= r4
  toDeBruijn t5 [] [] @?= r5
  toDeBruijn t6 [] [] @?= r6

unit_alphaEq = do
  alphaEq e1 e2 @?= True
  alphaEq e3 t3 @?= True
  alphaEq e4 t4 @?= True
  alphaEq e5 t5 @?= True
  alphaEq e1 t3 @?= False
  alphaEq e2 e3 @?= False
  alphaEq e1 t4 @?= False
  alphaEq e2 e5 @?= False

--sometimes works sometimes not :(
unit_CAS = do
  show (cas e sub) @?= "λy.x y"
