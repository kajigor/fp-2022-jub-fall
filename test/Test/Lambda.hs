module Test.Lambda where
import Test.HUnit (Assertion, assertBool, (@?=))

import Lambda

unit_show_string :: Assertion
unit_show_string = do
    (show true) @?= "λx.λy.x"
    (show add) @?= "λm.λn.λf.λx.m f (n f x)"
    (show mult) @?= "λm.λn.λf.m (n f)"
    (show or') @?= "λp.λq.p p q"
    (show (App (Abs "x" (App (Var "t") (Var "x"))) (Abs "y" (Var "y")))) @?= "(λx.t x) (λy.y)"
    (show (App (App (Var "t") (Var "x")) (Abs "y" (Var "y")))) @?= "t x (λy.y)"


-- λx.x
term1 = Abs "x" (Var "x")

-- λy.y
term2 = Abs "y" (Var "y")

-- λx.λy.x
term3 = Abs "x" (Abs "y" (Var "x"))

-- x (λx.x) (λx.λy.x)
term4 = Abs "y" (Abs "x" (App (App (Var "x") (Abs "x" (Var "x"))) (Abs "x" (Abs "y" (Var "x")))))

-- x (λt.t) (λa.λb.a)
term5 = Abs "y" (Abs "x" (App (App (Var "x") (Abs "t" (Var "t"))) (Abs "a" (Abs "b" (Var "a")))))

-- a (λt.t) (λx.λy.x)
term6 = Abs "a" (Abs "w" (App (App (Var "a") (Abs "t" (Var "t"))) (Abs "x" (Abs "y" (Var "x")))))

unit_alpha_eq :: Assertion
unit_alpha_eq = do
    (alphaEq term1 term2) @?= True
    (alphaEq term1 term3) @?= False
    (alphaEq term3 term4) @?= False
    (alphaEq term5 term4) @?= True
    (alphaEq term5 term6) @?= False

term1DeBrujin = AbsDB (VarDB 1)

term3DeBrujin = AbsDB (AbsDB (VarDB 2))

term4DeBrujin = AbsDB (AbsDB (AppDB (AppDB (VarDB 1) (AbsDB (VarDB 1))) (AbsDB (AbsDB (VarDB 2)))))

term6DeBruijn = AbsDB (AbsDB (AppDB (AppDB (VarDB 2) (AbsDB (VarDB 1))) (AbsDB (AbsDB (VarDB 2)))))

unit_to_DeBruijn :: Assertion
unit_to_DeBruijn = do
    (toDeBruijn term1) @?= term1DeBrujin
    (toDeBruijn term2) @?= term1DeBrujin
    (toDeBruijn term3) @?= term3DeBrujin
    (toDeBruijn term4) @?= term4DeBrujin
    (toDeBruijn term5) @?= term4DeBrujin
    (toDeBruijn term6) @?= term6DeBruijn
    ((toDeBruijn term4) == term6DeBruijn) @?= False


unit_from_DeBruijn :: Assertion
unit_from_DeBruijn = do
    (alphaEq (fromDeBruijn term1DeBrujin) term1) @?= True
    (alphaEq term2 (fromDeBruijn term1DeBrujin)) @?= True
    (alphaEq term3 (fromDeBruijn term3DeBrujin)) @?= True
    (alphaEq term4 (fromDeBruijn term4DeBrujin)) @?= True
    (alphaEq term5 (fromDeBruijn term4DeBrujin)) @?= True
    (alphaEq term6 (fromDeBruijn term6DeBruijn)) @?= True
    (alphaEq term4 (fromDeBruijn term6DeBruijn)) @?= False


unit_show_string_DeBruin :: Assertion
unit_show_string_DeBruin = do
    (show (toDeBruijn true)) @?= "λ.λ.2"
    (show (toDeBruijn add)) @?= "λ.λ.λ.λ.4 2 (3 2 1)"
    (show (toDeBruijn mult)) @?= "λ.λ.λ.3 (2 1)"
    (show (toDeBruijn or')) @?= "λ.λ.2 2 1"
    (show term6DeBruijn) @?= "λ.λ.2 (λ.1) (λ.λ.2)"

k = (Abs "x" (Abs "y" (Var "x")))
i = (Abs "x" (Var "x"))
s = (Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))))

-- (λx.x x) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
term_eval1 = App (Abs "x" (App (Var "x") (Var "x"))) (App (App (App k i) s) s)
-- (λy.λx.y x ((λx.x) x)) ((λx.λy.x) (λx.λy.λz.x z (y z)) (λx.x))
term_eval2 = App (Abs "y" (Abs "x" (App (App (Var "y") (Var "x")) (App i (Var "x"))))) (App (App k s) i)
-- (λf.λx.λy.f x (f y)) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)))
term_eval3 = App (Abs "f" (Abs "x" (Abs "y" (App (App (Var "f") (Var "x")) (App (Var "f") (Var "y")))))) (App (App k i) s)
-- (λx.(λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) x) ((λx.λy.λz.x z (y z)) x)
term_eval4 = App (Abs "x" (App (App (App k i) s) (Var "x"))) (App s (Var "x"))

-- λy.λz.(λx.λy.λz.x z (y z)) z (y z)
term_eval1_ByValue = Abs "y" (Abs "z" (App (App s (Var "z")) (App (Var "y") (Var "z"))))
-- λx.(λx.λy.λz.x z (y z)) x ((λx.x) x)
term_eval2_ByValue = Abs "x" (App (App s (Var "x")) (App i (Var "x")))
-- λx.λy.(λx.x) x ((λx.x) y)
term_eval3_ByValue = Abs "x" (Abs "y" (App (App i (Var "x")) (App i (Var "y"))))
-- λy.λz.x z (y z)
term_eval4_ByValue = (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

-- λy.λz.(λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)) z (y z)
term_eval1_ByName = Abs "y" (Abs "z" (App (App (App (App (App k i) s) s) (Var "z")) (App (Var "y") (Var "z"))))
-- λx.(λx.λy.x) (λx.λy.λz.x z (y z)) (λx.x) x ((λx.x) x)
term_eval2_ByName = Abs "x" (App (App (App (App k s) i) (Var "x")) (App i (Var "x")))
-- λx.λy.(λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) x ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) y)
term_eval3_ByName =  Abs "x" (Abs "y" (App (App (App (App k i) s) (Var "x")) (App (App (App k i) s) (Var "y"))))
-- λy.λz.x z (y z)
term_eval4_ByName = (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

-- λy.λz.λb.z b (y z b)
term_eval1_Normal = Abs "y" (Abs "z" (Abs "b" (App (App (Var "z") (Var "b")) (App (App (Var "y") (Var "z")) (Var "b")))))
-- λx.λz.x z (x z)
term_eval2_Normal = Abs "x" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "x") (Var "z"))))
-- λx.λy.x y
term_eval3_Normal = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
-- λy.λz.x z (y z)
term_eval4_Normal = (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

-- λy.λz.λb.z b (y z b)
term_eval1_Applicative = Abs "y" (Abs "z" (Abs "b" (App (App (Var "z") (Var "b")) (App (App (Var "y") (Var "z")) (Var "b")))))
-- λx.λz.x z (x z)
term_eval2_Applicative = Abs "x" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "x") (Var "z"))))
-- λx.λy.x y
term_eval3_Applicative = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
-- λy.λz.x z (y z)
term_eval4_Applicative = (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

unit_eval :: Assertion
unit_eval = do
    (alphaEq (eval CallByValue term_eval1) term_eval1_ByValue) @?= True
    (alphaEq (eval CallByValue term_eval2) term_eval2_ByValue) @?= True
    (alphaEq (eval CallByValue term_eval3) term_eval3_ByValue) @?= True
    (alphaEq (eval CallByValue term_eval4) term_eval4_ByValue) @?= True
    (alphaEq (eval CallByName term_eval1) term_eval1_ByName) @?= True
    (alphaEq (eval CallByName term_eval2) term_eval2_ByName) @?= True
    (alphaEq (eval CallByName term_eval3) term_eval3_ByName) @?= True
    (alphaEq (eval CallByName term_eval4) term_eval4_ByName) @?= True
    (alphaEq (eval NormalOrder term_eval1) term_eval1_Normal) @?= True
    (alphaEq (eval NormalOrder term_eval2) term_eval2_Normal) @?= True
    (alphaEq (eval NormalOrder term_eval3) term_eval3_Normal) @?= True
    (alphaEq (eval NormalOrder term_eval4) term_eval4_Normal) @?= True
    (alphaEq (eval ApplicativeOrder term_eval1) term_eval1_Applicative) @?= True
    (alphaEq (eval ApplicativeOrder term_eval2) term_eval2_Applicative) @?= True
    (alphaEq (eval ApplicativeOrder term_eval3) term_eval3_Applicative) @?= True
    (alphaEq (eval ApplicativeOrder term_eval4) term_eval4_Applicative) @?= True
    