{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Lambda where

import Test.Tasty.HUnit ((@=?), assertBool, Assertion, HasCallStack)
import Lambda (Lambda (Var, Abs, App), alphaEq, DeBruijn (AbsDB, VarDB, AppDB), toDeBruijn, fromDeBruijn)
import Lambda (Lambda (Var, Abs, App), alphaEq, DeBruijn (AbsDB, VarDB, AppDB), toDeBruijn, fromDeBruijn)

unit_showLambda :: IO ()
unit_showLambda = do
    "\\x.\\y.x" @=? show true
    "\\x.\\y.y" @=? show false
    "\\p.\\q.p q p" @=? show Test.Lambda.and
    "\\p.\\q.p p q" @=? show Test.Lambda.or
    "\\p.p (\\x.\\y.y) (\\x.\\y.x)" @=? show Test.Lambda.not
    "\\p.\\a.\\b.p a b" @=? show ifThenElse
    "\\f.\\x.x" @=? show zero
    "\\f.\\x.f x" @=? show one
    "\\f.\\x.f (f x)" @=? show two
    "\\f.\\x.f (f (f x))" @=? show three
    "\\f.\\x.f (f (f (f x)))" @=? show four
    "\\m.\\n.\\f.\\x.m f (n f x)" @=? show add
    "\\n.\\f.\\x.f (n f x)" @=? show successor
    "\\m.\\n.m (\\n.\\f.\\x.f (n f x)) n" @=? show add'
    "\\m.\\n.\\f.m (n f)" @=? show mult
    "\\m.\\n.m ((\\m.\\n.\\f.\\x.m f (n f x)) n) (\\f.\\x.x)" @=? show mult'

unit_showLambdaDB = do
    "\\ \\ 2" @=? show trueDB
    "\\ \\ 1" @=? show falseDB
    "\\ \\ 2 1 2" @=? show andDB
    "\\ \\ 2 2 1" @=? show orDB
    "\\ 1 (\\ \\ 1) (\\ \\ 2)" @=? show notDB
    "\\ \\ \\ 3 2 1" @=? show ifThenElseDB
    "\\ \\ 1" @=? show zeroDB
    "\\ \\ 2 1" @=? show oneDB
    "\\ \\ 2 (2 1)" @=? show twoDB
    "\\ \\ 2 (2 (2 1))" @=? show threeDB

unit_toDeBruijn = do
    trueDB @=? toDeBruijn true
    falseDB @=? toDeBruijn false
    andDB @=? toDeBruijn Test.Lambda.and
    orDB @=? toDeBruijn Test.Lambda.or
    notDB @=? toDeBruijn Test.Lambda.not
    ifThenElseDB @=? toDeBruijn ifThenElse
    zeroDB @=? toDeBruijn zero
    oneDB @=? toDeBruijn one
    twoDB @=? toDeBruijn two
    threeDB @=? toDeBruijn three
    freeVarsDB @=? toDeBruijn freeVars

unit_fromDeBruijn = do
    trueDB @=? toDeBruijn (fromDeBruijn trueDB)
    falseDB @=? toDeBruijn (fromDeBruijn falseDB)
    andDB @=? toDeBruijn (fromDeBruijn andDB)
    orDB @=? toDeBruijn (fromDeBruijn orDB)
    notDB @=? toDeBruijn (fromDeBruijn notDB)
    ifThenElseDB @=? toDeBruijn (fromDeBruijn ifThenElseDB)
    zeroDB @=? toDeBruijn (fromDeBruijn zeroDB)
    oneDB @=? toDeBruijn (fromDeBruijn oneDB)
    twoDB @=? toDeBruijn (fromDeBruijn twoDB)
    threeDB @=? toDeBruijn (fromDeBruijn threeDB)
    freeVarsDB @=? toDeBruijn (fromDeBruijn freeVarsDB)

assertAlphaEq :: HasCallStack => Lambda String -> Lambda String -> Assertion
assertAlphaEq x y = assertBool ("Should be alpha equal: " ++ show x ++ ", " ++ show y)
    (alphaEq x y)
assertNotAlphaEq :: HasCallStack => Lambda String -> Lambda String -> Assertion
assertNotAlphaEq x y = assertBool ("Should not be alpha equal: " ++ show x ++ ", " ++ show y)
    (Prelude.not $ alphaEq x y)

unit_alphaEq = do
    assertNotAlphaEq (Var "x") (Var "y")
    assertAlphaEq    (Var "x") (Var "x")
    assertAlphaEq    (Abs "x" (Var "x"))
                     (Abs "y" (Var "y"))
    assertAlphaEq    (Abs "x" (Var "z"))
                     (Abs "y" (Var "z"))
    assertNotAlphaEq (Abs "x" (Var "a"))
                     (Abs "y" (Var "b"))

    assertNotAlphaEq (Abs "x" (Var "x"))
                     (App (Var "x") (Var "x"))

    assertAlphaEq    (App (Abs "x" (Var "x")) (Abs "x" (Var "x")))
                     (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
    assertAlphaEq    (App (Abs "x" (Var "x")) (Abs "y" (Var "x")))
                     (App (Abs "x" (Var "x")) (Abs "z" (Var "x")))
    assertNotAlphaEq (App (Abs "x" (Var "x")) (Abs "y" (Var "a")))
                     (App (Abs "x" (Var "x")) (Abs "z" (Var "b")))

    assertAlphaEq    (Abs "x" (Abs "x" (Var "x")))
                     (Abs "y" (Abs "z" (Var "z")))
    assertNotAlphaEq (Abs "x" (Abs "x" (Var "x")))
                     (Abs "y" (Abs "z" (Var "y")))
    assertAlphaEq    (Abs "x" (Abs "a" (Var "x")))
                     (Abs "y" (Abs "z" (Var "y")))

    assertAlphaEq    (Abs "x" (App (Var "x") (Var "z"))) (Abs "y" (App (Var "y") (Var "z")))
    assertNotAlphaEq (Abs "x" (App (Var "x") (Var "z"))) (Abs "y" (App (Var "y") (Var "k")))


    assertAlphaEq    (Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y") (Var "z"))))))
                     (Abs "a" (Abs "b" (Abs "c" (App (Var "a") (App (Var "b") (Var "c"))))))
    assertNotAlphaEq (Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y") (Var "z"))))))
                     (Abs "a" (Abs "b" (Abs "c" (App (App (Var "a") (Var "b")) (Var "c")))))
    assertNotAlphaEq (Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y") (Var "z"))))))
                     (Abs "a" (Abs "b" (Abs "c" (App (Var "a") (App (Var "b") (Var "f"))))))

    assertAlphaEq    (Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y") (Var "f"))))))
                     (Abs "a" (Abs "b" (Abs "c" (App (Var "a") (App (Var "b") (Var "f"))))))
    assertAlphaEq    (Abs "a" (Abs "a" (Abs "b" (App (Var "a") (App (Var "a") (Var "b"))))))
                     (Abs "a" (Abs "b" (Abs "c" (App (Var "b") (App (Var "b") (Var "c"))))))
    assertNotAlphaEq (Abs "a" (Abs "a" (Abs "b" (App (Var "a") (App (Var "a") (Var "b"))))))
                     (Abs "a" (Abs "b" (Abs "c" (App (Var "a") (App (Var "a") (Var "c"))))))


-- true ≡ λx.λy.x
true = Abs "x" (Abs "y" (Var "x"))
-- true ≡ λ λ 2
trueDB = AbsDB (AbsDB (VarDB 2))

-- false ≡ λx.λy.y
false = Abs "x" (Abs "y" (Var "y"))
-- false = λ λ 1
falseDB = AbsDB (AbsDB (VarDB 1))

-- and ≡ λp.λq.p q p
and = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))
-- and ≡ λ λ 2 1 2
andDB = AbsDB (AbsDB (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 2)))

-- or ≡ λp.λq.p p q
or = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))
-- or = λ λ 2 2 1
orDB = AbsDB (AbsDB (AppDB (AppDB (VarDB 2) (VarDB 2)) (VarDB 1)))

-- not ≡ λp.p FALSE TRUE
not = Abs "p" (App (App (Var "p") false) true)
-- not = λ 1 (λ λ 1) (λ λ 2)
notDB = AbsDB (AppDB (AppDB (VarDB 1) (AbsDB (AbsDB (VarDB 1)))) (AbsDB (AbsDB (VarDB 2))))

-- ifThenElse ≡ λp.λa.λb.p a b
ifThenElse = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))
-- ifThenElse = λ λ λ 3 2 1
ifThenElseDB = AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 2)) (VarDB 1))))

-- zero ≡ λf.λx.x
zero = Abs "f" (Abs "x" (Var "x"))
-- zero = λ λ 1
zeroDB = AbsDB (AbsDB (VarDB 1))

-- one ≡ λf.λx.f x
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))
-- one ≡ λ λ 2 1
oneDB = AbsDB (AbsDB (AppDB (VarDB 2) (VarDB 1)))

-- two ≡ λf.λx.f (f x)
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))
-- two ≡ λ λ 2 (2 1)
twoDB = AbsDB (AbsDB (AppDB (VarDB 2) (AppDB (VarDB 2) (VarDB 1))))

-- three ≡ λf.λx.f (f (f x))
three =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))
-- three ≡ λ λ 2 (2 (2 1))
threeDB =  AbsDB (AbsDB (AppDB (VarDB 2) (AppDB (VarDB 2) (AppDB (VarDB 2) (VarDB 1)))))

--  four ≡ λf.λx.f (f (f (f x)))
four =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- add' ≡ λm.λn.m successor n
add' = Abs "m" (Abs "n" (App (App (Var "m") successor) (Var "n")))

-- mult ≡ λm.λn.λf.m (n f)
mult = Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f")))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- freeVars = λx.a (λy.(x b) (c y))
freeVars = Abs "x" (App (Var "a") (Abs "y" (App (App (Var "x") (Var "b")) (App (Var "c") (Var "y")))))
-- freeVarsDB = λ 2 (λ (2 4) (5 1))
freeVarsDB = AbsDB (AppDB (VarDB 2) (AbsDB (AppDB (AppDB (VarDB 2) (VarDB 4)) (AppDB (VarDB 5) (VarDB 1)))))
