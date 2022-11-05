module Test.Lambda where
import Test.Tasty
import Test.Tasty.HUnit
import Lambda

unit_show :: IO()
unit_show = do
    show (Var "x") @?= "x"
    show (App (Var "x") (Var "y")) @?= "x y"
    show (Abs "x" (Var "y")) @?= "\\x.y"
    show Lambda.true @?= "\\x.\\y.x"
    show Lambda.and @?= "\\p.\\q.p q p"
    show Lambda.two @?= "\\f.\\x.f (f x)"
    show Lambda.mult' @?= "\\m.\\n.m ((\\m.\\n.\\f.\\x.m f (n f x)) n) (\\f.\\x.x)"


-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

db1 :: Lambda String
db1 = Abs "x" (Abs "y" (Var "x"))

db2 :: Lambda String
db2 = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

db3 :: Lambda String
db3 = Abs "z" (App (Abs "y" (App (Var "y") (Abs "x" (Var "x")))) (Abs "x" (App (Var "z") (Var "x"))))

db1' :: Lambda String
db1' = Abs "y" (Abs "z" (Var "y"))

db1'' :: Lambda String
db1'' = Abs "y" (Abs "z" (Var "z"))

db2' :: Lambda String
db2' = Abs "a" (Abs "b" (Abs "c" (App (App (Var "a") (Var "c")) (App (Var "b") (Var "c")))))

unit_show_toDeBruijn :: IO()
unit_show_toDeBruijn = do
    show (toDeBruijn db1) @?= "\\ \\ 2"
    show (toDeBruijn db1') @?= "\\ \\ 2"
    show (toDeBruijn db1'') @?= "\\ \\ 1"
    show (toDeBruijn db2) @?= "\\ \\ \\ 3 1 (2 1)"
    show (toDeBruijn db2') @?= "\\ \\ \\ 3 1 (2 1)"
    show (toDeBruijn db3) @?= "\\ (\\ 1 (\\ 1)) (\\ 2 1)"



unit_alpha :: IO()
unit_alpha = do
    alphaEq db1 db1' @?= True
    alphaEq db1 db1'' @?= False
    alphaEq db1 db2' @?= False
    alphaEq db2 db2' @?= True