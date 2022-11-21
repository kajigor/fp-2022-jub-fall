module Test.Lambda where

import Test.Tasty.HUnit (Assertion, (@?=))
import Lambda
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

unit_show :: Assertion
unit_show = do
  show true @?= "\\x.\\y.x"
  show false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show zero @?= "\\f.\\x.x"
  show four @?= "\\f.\\x.f (f (f (f x)))"
  show add @?= "\\m.\\n.\\f.\\x.m f (n f x)"
  show (App (Abs "x" (Var "x y")) (Var "z")) @?= "(\\x.x y) z"
  show l4 @?= "\\x.x \\x.x"
  show withFree1 @?= "(\\x.z \\z.y x z) (y \\y.y \\w.w y)"


db1 = VarDB 1
l1 = Var "x"
r1 = Var 1
cas1 = Var "y"

-- λy. (λx. x y) (λz. y z) ≡ λ (λ 1 2) (λ 2 1)
db2 = AbsDB (AppDB (AbsDB (AppDB (VarDB 1) (VarDB 2))) (AbsDB (AppDB (VarDB 2) (VarDB 1))))
l2  = Abs "y" (App (Abs "x" (App (Var "x") (Var "y"))) (Abs "z" (App (Var "y") (Var "z"))))
r2  = Abs 1 (App (Abs 2 (App (Var 2) (Var 1))) (Abs 3 (App (Var 1) (Var 3))))


-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)
db3 = AbsDB (AppDB( AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 1)))) (AbsDB(AppDB (VarDB 2) (VarDB 1))))
l3 = Abs "z" (App(Abs "y" (App (Var "y") (Abs "x" (Var "x")))) (Abs "x" (App (Var "z") (Var "x"))))
r3 = Abs 1 (App(Abs 2 (App (Var 2) (Abs 3 (Var 3)))) (Abs 4 (App (Var 1) (Var 4))))

-- λx.x (λx. x) ≡ λ 1 (λ 1)
db4 = AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 1)))
l4 = Abs "x" (App (Var "x") (Abs "x" (Var "x")))
-- λx.x (λy. y)
l4_1 = Abs "x" (App (Var "x") (Abs "y" (Var "y")))
r4 = Abs 1 (App (Var 1) (Abs 2 (Var 2)))

unit_DeBruijne :: Assertion
unit_DeBruijne = do
  toDeBruijn l1 @?= db1 
  toDeBruijn l2 @?= db2
  toDeBruijn l3 @?= db3
  toDeBruijn l4 @?= db4
  toDeBruijn l4_1 @?= db4
  toDeBruijn withFree1 @?= dbWithFree1

  fromDeBruijn db1 @?= r1
  fromDeBruijn db2 @?= r2
  fromDeBruijn db3 @?= r3
  fromDeBruijn db4 @?= r4

unit_alphaEq :: Assertion
unit_alphaEq = do
  alphaEq l1 r1 @?= True
  alphaEq (fromDeBruijn db1) l1 @?= True
  alphaEq l2 r2 @?= True
  alphaEq (fromDeBruijn db2) l2 @?= True
  alphaEq l3 r3 @?= True
  alphaEq (fromDeBruijn  db3) l3 @?= True
  alphaEq l4 r4 @?= True
  alphaEq (fromDeBruijn  db4) l4 @?= True
  alphaEq l4 l4_1 @?= True

  alphaEq l1 r2 @?= False
  alphaEq l1 l2 @?= False
  alphaEq l1 r3 @?= False
  alphaEq l1 l3 @?= False
  alphaEq l1 l4 @?= False
  alphaEq l1 r4 @?= False

  alphaEq l2 r1 @?= False
  alphaEq l2 l1 @?= False
  alphaEq l2 r3 @?= False
  alphaEq l2 l3 @?= False
  alphaEq l2 l4 @?= False
  alphaEq l2 r4 @?= False
  
  alphaEq l3 r2 @?= False
  alphaEq l3 l2 @?= False
  alphaEq l3 r1 @?= False
  alphaEq l3 l1 @?= False
  alphaEq l3 l4 @?= False
  alphaEq l3 r4 @?= False

  alphaEq l4 r2 @?= False
  alphaEq l4 l2 @?= False
  alphaEq l4 r1 @?= False
  alphaEq l4 l1 @?= False
  alphaEq l4 l3 @?= False
  alphaEq l4 r3 @?= False


-- (λx. z (λz. y x z)) (y (λy. y (λw. w y)))
withFree1 = App (Abs "x" (App (Var "z") (Abs "z" (App (App (Var "y") (Var "x")) (Var "z"))))) (App (Var "y") (Abs "y" (App (Var "y") (Abs "w" (App (Var "w") (Var "y"))))))
casMap1_0 :: Map.Map [Char] [Char]
casMap1_0 = Map.fromList [("x", "s1"), ("w", "s2")]
sub1_0 = Subst "y" (App (Var "x") (Var "w")) casMap1_0
casResult1_0 = App (Abs "s1" (App (Var "z") (Abs "z" (App (App (App (Var "x") (Var "w")) (Var "s1")) (Var "z"))))) (App (App (Var "x") (Var "w")) (Abs "y" (App (Var "y") (Abs "w" (App (Var "w") (Var "y"))))))

dbWithFree1 = AppDB (AbsDB (AppDB (VarDB 2) (AbsDB (AppDB (AppDB (VarDB 4) (VarDB 2)) (VarDB 1))))) (AppDB (VarDB 2) (AbsDB (AppDB (VarDB 1) (AbsDB (AppDB (VarDB 1) (VarDB 2))))))

unit_cas :: Assertion
unit_cas = do
  fv withFree1 @?= Set.fromList ["y", "z"]

  cas l1 (Subst "x" (Var "y") (Map.singleton "y" "z")) @?= cas1
  cas l2 (Subst "y" (Var "Solzhenitsyn") (Map.singleton "Solzhenitsyn" "Gulag")) @?= l2

  cas withFree1 sub1_0 @?= casResult1_0
