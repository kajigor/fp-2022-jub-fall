module Test.Lambda where

import Lambda
import Test.HUnit (Assertion, assertBool, (@?=))


k :: Lambda String
k = Abs "x" (Abs "y" (Var "x"))
i :: Lambda String
i = Abs "x" (Var "x")
s :: Lambda String
s = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x")(Var "z")) (App (Var "y") (Var "z")))))
kiss :: Lambda String
kiss = App (App (App k i) s) s

expr1 :: Lambda String
expr1 = App (Abs "y" (Abs "x" (App (Var "x") (Var "y")))) (Var "x")

unit_show :: Assertion
unit_show = do
  "λx.λy.x" @?= show true
  "λx.λy.y" @?= show false
  "λp.λq.p q p" @?= show Lambda.and
  "λp.λq.p p q" @?= show Lambda.or
  "λp.p (λx.λy.y) (λx.λy.x)" @?= show Lambda.not
  "λp.λa.λb.p a b" @?= show Lambda.ifThenElse
  "λf.λx.x" @?= show Lambda.zero
  "λf.λx.f x" @?= show Lambda.one
  "λf.λx.f (f x)" @?= show Lambda.two
  "λf.λx.f (f (f x))" @?= show Lambda.three
  "λf.λx.f (f (f (f x)))" @?= show Lambda.four
  "λm.λn.λf.λx.m f (n f x)" @?= show Lambda.add
  "λn.λf.λx.f (n f x)" @?= show Lambda.successor
  "λm.λn.m (λn.λf.λx.f (n f x)) n" @?= show Lambda.add'
  "λm.λn.λf.m (n f)" @?= show Lambda.mult
  "λm.λn.m ((λm.λn.λf.λx.m f (n f x)) n) (λf.λx.x)" @?= show Lambda.mult'
  
  "(λy.λx.x y) x" @?= show expr1

unit_alphaEq :: Assertion
unit_alphaEq = do
  assertBool "simple fail" $ Prelude.not $ alphaEq (Var "a") (Var "b")
  assertBool "simple alpha eq" $ alphaEq (Abs "a" (Var "a")) (Abs "b" (Var "b"))
  assertBool "long alpha eq" $ alphaEq (Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))) (Abs "w" (Abs "ewee" (App (App (Var "w") (Var "w")) (Var "ewee"))))

unit_debruijn_show :: Assertion
unit_debruijn_show = do
  "λ λ 1" @?= show (toDeBruijn true)
  "λ λ 2" @?= show (toDeBruijn false)
  "λ λ 1 2 1" @?= show (toDeBruijn Lambda.and)
  "λ λ 1 1 2" @?= show (toDeBruijn Lambda.or)
  "λ 1 (λ λ 3) (λ λ 4)" @?= show (toDeBruijn Lambda.not)
  "λ λ λ 1 2 3" @?= show (toDeBruijn Lambda.ifThenElse)
  "λ λ 2" @?= show (toDeBruijn Lambda.zero)
  "λ λ 1 2" @?= show (toDeBruijn Lambda.one)
  "λ λ 1 (1 2)" @?= show (toDeBruijn Lambda.two)
  "λ λ 1 (1 (1 2))" @?= show (toDeBruijn Lambda.three)
  "λ λ 1 (1 (1 (1 2)))" @?= show (toDeBruijn Lambda.four)
  "λ λ λ λ 1 3 (2 3 4)" @?= show (toDeBruijn Lambda.add)
  "λ λ λ 2 (1 2 3)" @?= show (toDeBruijn Lambda.successor)
  -- "λ λ 1 (λ λ λ 4 (3 4 5)) 3" @?= (show $ toDeBruijn Lambda.add') invalid
  "λ λ λ 1 (2 3)" @?= show (toDeBruijn Lambda.mult)
  -- "λ λ 1 (λ λ λ λ 3 5 (4 5 6) 4) (λ λ 8)" @?= (show $ toDeBruijn Lambda.mult') invalid

  "(λ λ 2 1) 2" @?= show (toDeBruijn expr1)

debruijnEquiv :: Lambda String -> Bool
debruijnEquiv x = alphaEq x ((fromDeBruijn $ toDeBruijn x) :: Lambda String)

unit_debruijin_back_and_forth :: Assertion
unit_debruijin_back_and_forth = do
  assertBool "" $ debruijnEquiv true
  assertBool "" $ debruijnEquiv false
  assertBool "" $ debruijnEquiv Lambda.and
  assertBool "" $ debruijnEquiv Lambda.or
  assertBool "" $ debruijnEquiv Lambda.not
  assertBool "" $ debruijnEquiv Lambda.ifThenElse
  assertBool "" $ debruijnEquiv Lambda.zero
  assertBool "" $ debruijnEquiv Lambda.one
  assertBool "" $ debruijnEquiv Lambda.two
  assertBool "" $ debruijnEquiv Lambda.three
  assertBool "" $ debruijnEquiv Lambda.four
  assertBool "" $ debruijnEquiv Lambda.add
  assertBool "" $ debruijnEquiv Lambda.successor
  -- assertBool "" $ debruijnEquiv Lambda.add' invalid
  assertBool "" $ debruijnEquiv Lambda.mult
  -- assertBool "" $ debruijnEquiv Lambda.mult' invalid

substEquiv :: Listable a => Lambda a -> Subst a -> Bool
substEquiv x subst = alphaEq x (cas x subst)

brokenAdd :: Lambda String
brokenAdd = Abs "m" (Abs "n" (Abs "B" (Abs "A" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

unit_cas :: Assertion
unit_cas = do
  assertBool "" $ substEquiv true $ Subst ("y", Var "f")
  assertBool "" $ substEquiv true $ Subst ("y", Var "x")
  assertBool "" $ substEquiv Lambda.add $ Subst ("n", Var "m")
  assertBool "" $ alphaEq (cas brokenAdd $ Subst ("f", Abs "A" (Var "A"))) (cas brokenAdd $ Subst ("f", Abs "n" (Var "n")))


unit_eval :: Assertion
unit_eval = do
  "λx.λy.λz.x z (y z)" @?= show (eval CallByName kiss)
  "λx.λy.λz.x z (y z)" @?= show (eval CallByValue kiss)
  "λx.λy.λz.x z (y z)" @?= show (eval NormalOrder kiss)
  "λx.λy.λz.x z (y z)" @?= show (eval ApplicativeOrder kiss)

  "λa.a x" @?= show (eval CallByName expr1)
  "λa.a x" @?= show (eval CallByValue expr1)
  "λa.a x" @?= show (eval NormalOrder expr1)
  "λa.a x" @?= show (eval ApplicativeOrder expr1)
