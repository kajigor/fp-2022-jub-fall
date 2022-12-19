module Test.Reductions where

import Test.Tasty
import Test.Tasty.HUnit

import Reductions
import Lambda
import Data.Maybe

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

-- ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z))) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
term_eval1_ByValue = App (Abs "x" (App (Var "x") (Var "x"))) (App (App (Abs "y" (Abs "x" (Var "x"))) s) s)
-- (λy.λx.y x ((λx.x) x)) ((λy.(λx.λy.λz.x z (y z))) (λx.x))
term_eval2_ByValue = App (Abs "y" (Abs "x" (App (App (Var "y") (Var "x")) (App i (Var "x"))))) (App (Abs "y" s) i)
-- (λf.λx.λy.f x (f y)) ((λy.(λx.x))  (λx.λy.λz.x z (y z)))
term_eval3_ByValue = App (Abs "f" (Abs "x" (Abs "y" (App (App (Var "f") (Var "x")) (App (Var "f") (Var "y")))))) (App (Abs "y" (Abs "x" (Var "x"))) s)
-- (λx.(λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) x) (λy.λz.x z (y z))
term_eval4_ByValue = App (Abs "x" (App (App (App k i) s) (Var "x"))) (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

-- ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z))) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
term_eval1_ByName = App (App (App (App k i) s) s) (App (App (App k i) s) s)
-- (λx. ((λx.λy.x) (λx.λy.λz.x z (y z)) (λx.x)) x ((λx.x) x)) 
term_eval2_ByName = ((Abs "x" (App (App ((App (App k s) i)) (Var "x")) (App i (Var "x")))))
-- (λx.λy.((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z))) x (((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z))) y)) 
term_eval3_ByName = (Abs "x" (Abs "y" (App (App (App (App k i) s) (Var "x")) (App (App (App k i) s) (Var "y"))))) 
-- (λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) ((λx.λy.λz.x z (y z)) x) 
term_eval4_ByName = (App (App (App k i) s) ((App s (Var "x"))))

-- ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z))) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
term_eval1_Normal= App (App (App (App k i) s) s) (App (App (App k i) s) s)
-- (λx.((λx.λy.x) (λx.λy.λz.x z (y z)) (λx.x)) x ((λx.x) x))
term_eval2_Normal = (Abs "x" (App (App ((App (App k s) i)) (Var "x")) (App i (Var "x"))))
-- (λx.λy.((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z))) x (((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z))) y)) 
term_eval3_Normal = (Abs "x" (Abs "y" (App (App (App (App k i) s) (Var "x")) (App (App (App k i) s) (Var "y"))))) 
-- (λx.(λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) x) ((λx.λy.λz.x z (y z)) x)
term_eval4_Normal = (App (App (App k i) s) (App s (Var "x")))

-- (λx.x x) ((λy.(λx.x)) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
term_eval1_Applicative =  App (Abs "x" (App (Var "x") (Var "x"))) (App (App (Abs "y" (Abs "x" (Var "x"))) s) s)
-- (λy.λx.y x (x)) ((λx.λy.x) (λx.λy.λz.x z (y z)) (λx.x))
term_eval2_Applicative = App (Abs "y" (Abs "x" (App (App (Var "y") (Var "x")) (Var "x")))) (App (App k s) i)
-- (λf.λx.λy.f x (f y)) ((λy.(λx.x)) (λx.λy.λz.x z (y z)))
term_eval3_Applicative = App (Abs "f" (Abs "x" (Abs "y" (App (App (Var "f") (Var "x")) (App (Var "f") (Var "y")))))) (App (Abs "y" (Abs "x" (Var "x"))) s)
-- (λx.(λy.(λx.x)) (λx.λy.λz.x z (y z)) x) ((λx.λy.λz.x z (y z)) x)
term_eval4_Applicative = App (Abs "x" (App (App (Abs "y" (Abs "x" (Var "x"))) s) (Var "x"))) (App s (Var "x"))

unit_eval :: Assertion
unit_eval = do
    (alphaEq (fromJust (reductor CallByValue term_eval1)) term_eval1_ByValue) @?= True
    (alphaEq (fromJust (reductor CallByValue term_eval2)) term_eval2_ByValue) @?= True
    (alphaEq (fromJust (reductor CallByValue term_eval3)) term_eval3_ByValue) @?= True
    (alphaEq (fromJust (reductor CallByValue term_eval4)) term_eval4_ByValue) @?= True
    (alphaEq (fromJust (reductor CallByName term_eval1)) term_eval1_ByName) @?= True
    (alphaEq (fromJust (reductor CallByName term_eval2)) term_eval2_ByName) @?= True
    (alphaEq (fromJust (reductor CallByName term_eval3)) term_eval3_ByName) @?= True
    (alphaEq (fromJust (reductor CallByName term_eval4)) term_eval4_ByName) @?= True
    (alphaEq (fromJust (reductor NormalOrder term_eval1)) term_eval1_Normal) @?= True
    (alphaEq (fromJust (reductor NormalOrder term_eval2)) term_eval2_Normal) @?= True
    (alphaEq (fromJust (reductor NormalOrder term_eval3)) term_eval3_Normal) @?= True
    (alphaEq (fromJust (reductor NormalOrder term_eval4)) term_eval4_Normal) @?= True
    (alphaEq (fromJust (reductor ApplicativeOrder term_eval1)) term_eval1_Applicative) @?= True
    (alphaEq (fromJust (reductor ApplicativeOrder term_eval2)) term_eval2_Applicative) @?= True
    (alphaEq (fromJust (reductor ApplicativeOrder term_eval3)) term_eval3_Applicative) @?= True
    (alphaEq (fromJust (reductor ApplicativeOrder term_eval4)) term_eval4_Applicative) @?= True
 

props :: [TestTree]
props = 
  [testCase "unit_eval" unit_eval]