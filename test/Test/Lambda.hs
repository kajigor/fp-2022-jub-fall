module Test.Lambda where
import Test.Tasty.HUnit (assertBool)

import Lambda
import Lambda (DeBruijn(..), toDeBruijn)

l1 = Abs "x" (Var "x")
l2 = Abs "x" (Abs "y" (Var "z"))
l3 = Abs "x" (App (Var "x") (Abs "y" (Var "z")))
l4 = App (App (Var "x") (Var "y")) (Var "z")
l5 = App (Abs "x" (Abs "y" (Var "x"))) (Var "y")
l6 = App (Var "x") (App (Var "y") (Var "z"))
l7 = App (Abs "x" (Var "x")) (App (Var "y") (Var "z"))
l8 = Abs "z" (App (Abs "y" (App (Var "y") (Abs "x" (Var "x")))) (Abs "x" (App (Var "z") (Var "x"))))

d1 = AbsDB (VarDB 1)
d2 = AbsDB (AbsDB (VarDB 3))
d3 = AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 3)))
d4 = AppDB (AppDB (VarDB 1) (VarDB 2)) (VarDB 3)
d5 = AppDB (AbsDB (AbsDB (VarDB 2))) (VarDB 3)
d6 = AppDB (VarDB 1) (AppDB (VarDB 2) (VarDB 3))
d7 = AppDB (AbsDB (VarDB 1)) (AppDB (VarDB 2) (VarDB 3))
d8 = AbsDB (AppDB (AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 1)))) (AbsDB (AppDB (VarDB 2) (VarDB 1))))

unit_show :: IO ()
unit_show = do
    assertBool "\\x.(x) == \\x.x" (show l1 == "\\x.x")
    assertBool "\\x.(\\y.z) == \\x.\\y.z" (show l2 == "\\x.\\y.z")
    assertBool "\\x.(x \\y.z) == \\x.x \\y.z" (show l3 == "\\x.x \\y.z")
    assertBool "(x y) z == x y z" (show l4 == "x y z")
    assertBool "(\\x.\\y.x) y /= \\x.\\y.x y" (show l5 == "(\\x.\\y.x) y")
    assertBool "x (y z) /= x y z" (show l6 == "x (y z)")
    assertBool "(\\x.x) (y z) /= \\x.x y z /= (\\x.x) y z /= \\x.x (y z)" (show l7 == "(\\x.x) (y z)")


unit_showDB = do
    assertBool "λx. λy. x ≡ λ λ 2" (show (AbsDB (AbsDB (VarDB 2))) == "λ λ 2")
    assertBool "λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)" (show (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (VarDB 2) (VarDB 1)))))) == "λ λ λ 3 1 (2 1)")
    assertBool "λz.(λy.y λx.x) λx.z x ≡ λ (λ 1 λ 1) λ 2 1" (show d8 == "λ (λ 1 λ 1) λ 2 1")
    assertBool "λx.x == λ 1"                (show d1 == "λ 1")
    assertBool "λx.λy.z == λ λ 3"           (show d2 == "λ λ 3")
    assertBool "λx.x λy.z == λ 1 λ 3"       (show d3 == "λ 1 λ 3")
    assertBool "x y z == 1 2 3"             (show d4 == "1 2 3")
    assertBool "(λx.λy.x) y = (λ λ 2) 3"    (show d5 == "(λ λ 2) 3")
    assertBool "x (y z) = 1 (2 3)"          (show d6 == "1 (2 3)")
    assertBool "(λx.x) (y z) = (λ 1) (2 3)" (show d7 == "(λ 1) (2 3)")

unit_lambdaToDB = do
    assertBool "x == 1"                     (toDeBruijn (Var "x") == VarDB 0)
    assertBool "λx.x == λ 1"                (toDeBruijn l1 == d1)
    assertBool "λz.(λy.y λx.x) λx.z x ≡ λ (λ 1 λ 1) λ 2 1" (toDeBruijn l8 == d8)
    -- assertBool "λx.λy.z == λ λ 3"           (toDeBruijn l2 == d2)
    -- assertBool "λx.x λy.z == λ 1 λ 3"       (toDeBruijn l3 == d3)
    -- assertBool "x y z == 1 2 3"             (toDeBruijn l4 == d4)
    -- assertBool "(λx.λy.x) y = (λ λ 2) 3"    (toDeBruijn l5 == d5)
    -- assertBool "x (y z) = 1 (2 3)"          (toDeBruijn l6 == d6)
    -- assertBool "(λx.x) (y z) = (λ 1) (2 3)" (toDeBruijn l7 == d7)

