module Test.Lambda where

import Lambda
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)

unit_show = do
    show true @?= "\\x.\\y.x"
    show false @?= "\\x.\\y.y"
    show Lambda.and @?= "\\p.\\q.p q p"
    show Lambda.or @?= "\\p.\\q.p p q"
    show one @?= "\\f.\\x.f x"
    show three @?= "\\f.\\x.f (f (f x))"
    show successor @?= "\\n.\\f.\\x.f (n f x)"
    show (Abs "y" (App (Var "y") (Abs "x" (Var "x")))) @?= "\\y.y (\\x.x)"

unit_alpha_eq = do
    assertBool "same lambdas" $ alphaEq three three
    assertBool "same lambdas" $ alphaEq (Abs "x" (Var "x")) (Abs "y" (Var "y"))
    assertBool "diff lambdas" $ Prelude.not $ alphaEq (Abs "x" (Var "x")) (Abs "x" (Var "y"))
    assertBool "diff lambdas" $ Prelude.not $ alphaEq (Abs "x" (Var "x")) (Abs "y" (Var "x"))
    assertBool "same lambdas" $ alphaEq (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Abs "y" (Abs "x" (App (Var "y") (Var "x"))))
    assertBool "diff lambdas" $ Prelude.not $ alphaEq (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Abs "y" (Abs "x" (App (Var "x") (Var "y"))))
     
unit_cas = do
    cas (Var "x") (Subst "x" true) @?= true
    cas (Var "x") (Subst "y" true) @?= (Var "x")
    cas (Abs "x" (App (Var "x") (Var "y"))) (Subst "y" (Var "x")) @?= Abs "a" (App (Var "a") (Var "x"))
    cas (Abs "x" successor) (Subst "x" false) @?= Abs "x" successor
    cas (App (Var "x") (Var "y")) (Subst "x" true) @?= App true (Var "y")
    cas (Abs "y" (App (Var "x") (Var "y"))) (Subst "x" true) @?= Abs "y" (App true (Var "y"))

check_DeBruijn :: Maybe DeBruijn -> String -> Assertion
check_DeBruijn Nothing _ = assertBool "Term should not be Nothing" False
check_DeBruijn (Just term) s = show term @?= s

unit_de_bruijn = do
    check_DeBruijn ( toDeBruijn (Abs "x" (Abs "y" (Var "x"))) ) "\\.\\.1"
    check_DeBruijn ( toDeBruijn (Abs "x" (Abs "y" (Var "y"))) ) "\\.\\.0"
    check_DeBruijn ( toDeBruijn (Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))) ) "\\.\\.\\.2 0 (1 0)"

    fromDeBruijn (AbsDB $ AbsDB $ VarDB 1) @?= Abs "a" (Abs "aa" (Var "a"))
    fromDeBruijn (AbsDB $ AbsDB $ VarDB 0) @?= Abs "a" (Abs "aa" (Var "aa"))

unit_eval = do
    show (eval CallByName (App (Abs "x" $ Var "x") (Var "y"))) @?= "y"
    show (eval CallByName $ App (App true $ Var "a") (Var "b")) @?= "a"
    show (eval CallByName $ App (Var "x") (App true $ Var "a")) @?= "x ((\\x.\\y.x) a)"

    show (eval NormalOrder (App (Abs "x" $ Var "x") (Var "y"))) @?= "y"
    show (eval NormalOrder $ App (App true $ Var "a") (Var "b")) @?= "a"
    show (eval NormalOrder $ App (Var "x") (App true $ Var "a")) @?= "x (\\y.a)"

    show (eval CallByValue (App (Abs "x" $ Var "x") (Var "y"))) @?= "y"
    show (eval CallByValue $ App (App true $ Var "a") (Var "b")) @?= "a"
    show (eval CallByValue $ App (Var "x") (App true $ Var "a")) @?= "x (\\y.a)"

    show (eval ApplicativeOrder (App (Abs "x" $ Var "x") (Var "y"))) @?= "y"
    show (eval ApplicativeOrder $ App (App true $ Var "a") (Var "b")) @?= "a"
    show (eval ApplicativeOrder $ App (Var "x") (App true $ Var "a")) @?= "x (\\y.a)"

