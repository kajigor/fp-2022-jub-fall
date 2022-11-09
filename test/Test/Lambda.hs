{-# LANGUAGE ImplicitParams #-}

module Test.Lambda where


import Test.Tasty.HUnit (Assertion, (@?=))
import Test.HUnit.Approx ((@?~))

import Lambda

l0 = Var "x"
l1 = Abs "x" (Var "x")
l2 = App (Var "x") (Var "y")
l3 = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "y")) (Var "z"))))
l4 = App (App (Var "a") (Var "b")) (App (Var "c") (Var "d"))
l5 = App (Var "a") (App (Var "b") (App (Var "c") (Var "d")))
l6 = App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d")

y = Abs "f" (App (Abs "x" (App (Var "f") (App (Var "x") (Var "x"))))(Abs "x" (App (Var "f") (App (Var "x") (Var "x")))))


unit_showString = do
	show l0 @?= "x"
	show l1 @?= "\\x.x"
	show l2 @?= "x y"
	show l3 @?= "\\x.\\y.\\z.x y z"
	show l4 @?= "a b (c d)"
	show l5 @?= "a (b (c d))"
	show l6 @?= "a b c d"

	show y @?= "\\f.(\\x.f (x x)) (\\x.f (x x))"

data MyTestType = A | B | C
	deriving (Show)
k0 = A
k3 = Abs A (Abs B (Abs C (App (App (Var A) (Var B)) (Var C))))
unit_showNotString = do
	show k0 @?= "A"
	show k3 @?= "\\A.\\B.\\C.A B C"

-- db0 = AbsDB (AbsDB (VarDB 2))
-- db1 = AbsDB ( AbsDB ( AbsDB ( AppDB  (AppDB (VarDB 3) (VarDB 1)) (AppDB (VarDB 2) (VarDB 1))  ) ))
-- db2 = AbsDB (AppDB (AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 1)))) (AbsDB (AppDB (VarDB 2) (VarDB 1))))
-- unit_showDeBruijn = do
-- 	show db0 @?= "\\.\\.2"
-- 	show db1 @?= "\\.\\.\\.3 1 (2 1)"
-- 	show db2 @?= "\\.(\\.1 (\\.1)) (\\.2 1)"
