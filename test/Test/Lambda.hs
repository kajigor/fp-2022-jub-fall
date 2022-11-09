module Test.Lambda where
import Test.Tasty.HUnit (assertBool)

import Lambda

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


unit_show = do
    assertBool "λx.(x) == λx.x"           (show l1 == "λx.x")
    assertBool "λx.(λy.z) == λx.λy.z"     (show l2 == "λx.λy.z")
    assertBool "λx.(x λy.z) == λx.x λy.z" (show l3 == "λx.x λy.z")
    assertBool "(x y) z == x y z"         (show l4 == "x y z")
    assertBool "(λx.λy.x) y /= λx.λy.x y" (show l5 == "(λx.λy.x) y")
    assertBool "x (y z) /= x y z"         (show l6 == "x (y z)")
    assertBool "(λx.x) (y z) /= λx.x y z /= (λx.x) y z /= λx.x (y z)" (show l7 == "(λx.x) (y z)")

unit_showDB = do
    assertBool "λx. λy. x ≡ λ λ 2"                         (show (AbsDB (AbsDB (VarDB 2))) == "λ λ 2")
    assertBool "λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)"   (show (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (VarDB 2) (VarDB 1)))))) == "λ λ λ 3 1 (2 1)")
    assertBool "λz.(λy.y λx.x) λx.z x ≡ λ (λ 1 λ 1) λ 2 1" (show d8 == "λ (λ 1 λ 1) λ 2 1")
    assertBool "λx.x == λ 1"                (show d1 == "λ 1")
    assertBool "λx.λy.z == λ λ 3"           (show d2 == "λ λ 3")
    assertBool "λx.x λy.z == λ 1 λ 3"       (show d3 == "λ 1 λ 3")
    assertBool "x y z == 1 2 3"             (show d4 == "1 2 3")
    assertBool "(λx.λy.x) y = (λ λ 2) 3"    (show d5 == "(λ λ 2) 3")
    assertBool "x (y z) = 1 (2 3)"          (show d6 == "1 (2 3)")
    assertBool "(λx.x) (y z) = (λ 1) (2 3)" (show d7 == "(λ 1) (2 3)")

unit_lambdaToDb = do
    assertBool "x == 3"                     (toDeBruijn (Var "x") [("x", 3)] == VarDB 3)
    assertBool "λx.x == λ 1"                (toDeBruijn l1 [] == d1)
    assertBool "λz.(λy.y λx.x) λx.z x ≡ λ (λ 1 λ 1) λ 2 1" (toDeBruijn l8 [] == d8)
    assertBool "λx.λy.z == λ λ 3"           (toDeBruijn l2 [("z", 3)] == d2)
    assertBool "λx.x λy.z == λ 1 λ 3"       (toDeBruijn l3 [("z", 3)] == d3)
    assertBool "x y z == 1 2 3"             (toDeBruijn l4 [("x", 1), ("y", 2), ("z", 3)] == d4)
    assertBool "(λx.λy.x) y = (λ λ 2) 3"    (toDeBruijn l5 [("y", 3)] == d5)
    assertBool "x (y z) = 1 (2 3)"          (toDeBruijn l6 [("x", 1), ("y", 2), ("z", 3)] == d6)
    assertBool "(λx.x) (y z) = (λ 1) (2 3)" (toDeBruijn l7 [("y", 2), ("z", 3)] == d7)

unit_dbToLambda = do
    assertBool "3 == x"                     (fromDeBruijn (VarDB 3) [(3, "x")] [] == Var "x")
    assertBool "λ 1 == λx.x"                (fromDeBruijn d1 [] ["x"] == l1)
    assertBool "λ λ 3 == λx.λy.z"           (fromDeBruijn d2 [(3, "z")] ["x", "y"] == l2)
    assertBool "1 2 3 == x y z"             (fromDeBruijn d4 [(1, "x"), (2, "y"), (3, "z")] [] == l4)
    assertBool "(λx.λy.x) y = (λ λ 2) 3"    (fromDeBruijn d5 [(3, "y")] ["x", "y"] == l5)

unit_alphaEq = do
    assertBool "x /= y"       (Prelude.not $ alphaEq (Var "x") (Var "y"))
    assertBool "x == x"       (alphaEq (Var "x") (Var "x"))
    assertBool "λx.x == λy.y" (alphaEq (Abs "x" (Var "x")) (Abs "y" (Var "y")))
    assertBool "λx.x /= λy.x" (Prelude.not $ alphaEq (Abs "x" (Var "x")) (Abs "y" (Var "x")))
    assertBool "λx.y /= λy.x" (Prelude.not $ alphaEq (Abs "x" (Var "y")) (Abs "y" (Var "x")))
