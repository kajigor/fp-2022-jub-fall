module Test.Lambda where

import Lambda
import Test.Tasty.HUnit ((@?=), assertBool)

x :: Lambda String
x = Var "x"
y :: Lambda String
y = Var "y"
z :: Lambda String
z = Var "z"

var1 :: DeBruijn
var1 = VarDB 1
var2 :: DeBruijn
var2 = VarDB 2
var3 :: DeBruijn
var3 = VarDB 3

-- λz.(λy.y (λx.x)) (λx.z x)
ld1 :: Lambda String
ld1 = Abs "z" $ App (Abs "y" $ App y (Abs "x" x)) (Abs "x" $ App z x)
-- λ (λ 1 (λ 1)) (λ 2 1)
db1 :: DeBruijn
db1 = AbsDB $ AppDB (AbsDB $ AppDB var1 (AbsDB var1)) (AbsDB $ AppDB var2 var1)

-- λx.λy.λz.x z (y z)
ld2 :: Lambda String
ld2 = Abs "x" $ Abs "y" $ Abs "z" $ App (App x z) (App y z)
-- λ λ λ 3 1 (2 1)
db2 :: DeBruijn
db2 = AbsDB $ AbsDB $ AbsDB $ AppDB (AppDB var3 var1) (AppDB var2 var1)

-- λx.λy.x (y z)
ld3 :: Lambda String
ld3 = Abs "x" $ Abs "y" $ App x (App y z)
-- λ λ 2 (1 3)
db3 :: DeBruijn
db3 = AbsDB $ AbsDB $ AppDB var2 (AppDB var1 var3)

unit_show = do
    show x @?= "x"
    show true @?= "\\x.\\y.x"
    show false @?= "\\x.\\y.y"
    show Lambda.and @?= "\\p.\\q.p q p"
    show Lambda.or @?= "\\p.\\q.p p q"
    show ifThenElse @?= "\\p.\\a.\\b.p a b"
    show zero @?= "\\f.\\x.x"
    show one @?= "\\f.\\x.f x"
    show two @?= "\\f.\\x.f (f x)"
    show three @?= "\\f.\\x.f (f (f x))"
    show four @?= "\\f.\\x.f (f (f (f x)))"
    show add @?= "\\m.\\n.\\f.\\x.m f (n f x)"
    show successor @?= "\\n.\\f.\\x.f (n f x)"
    show mult @?= "\\m.\\n.\\f.m (n f)"
    show ld1 @?= "\\z.(\\y.y (\\x.x)) (\\x.z x)"
    show ld2 @?= "\\x.\\y.\\z.x z (y z)"
    show ld3 @?= "\\x.\\y.x (y z)"

unit_toDeBruijn = do
    show (toDeBruijn x) @?= "1"
    show (toDeBruijn true) @?= "\\ \\ 2"
    show (toDeBruijn false) @?= "\\ \\ 1"
    show (toDeBruijn Lambda.and) @?= "\\ \\ 2 1 2"
    show (toDeBruijn Lambda.or) @?= "\\ \\ 2 2 1"
    show (toDeBruijn ifThenElse) @?= "\\ \\ \\ 3 2 1"
    show (toDeBruijn four) @?= "\\ \\ 2 (2 (2 (2 1)))"
    show (toDeBruijn successor) @?= "\\ \\ \\ 2 (3 2 1)"
    show (toDeBruijn ld1) @?= show db1
    show (toDeBruijn ld2) @?= show db2
    show (toDeBruijn ld3) @?= show db3

fromDeBruijnStr :: DeBruijn -> String
fromDeBruijnStr a = show s
    where
        s :: Lambda String
        s = fromDeBruijn a

unit_fromDeBruijn = do
    fromDeBruijnStr var1 @?= "a"
    fromDeBruijnStr (toDeBruijn true) @?= "\\a.\\b.a"
    fromDeBruijnStr (toDeBruijn four) @?= "\\a.\\b.a (a (a (a b)))"
    fromDeBruijnStr db1 @?= "\\a.(\\b.b (\\c.c)) (\\b.a b)"
    fromDeBruijnStr db2 @?= "\\a.\\b.\\c.a c (b c)"
    fromDeBruijnStr db3 @?= "\\a.\\b.a (b c)"

unit_alphaEq = do
    assertBool "equal" (alphaEq (fromDeBruijn (toDeBruijn true)) true)
    assertBool "equal" (alphaEq (fromDeBruijn (toDeBruijn false)) false)
    assertBool "equal" (alphaEq (fromDeBruijn (toDeBruijn three)) three)
    assertBool "equal" (alphaEq (fromDeBruijn (toDeBruijn successor)) successor)
    assertBool "equal" (alphaEq (fromDeBruijn db1) ld1)
    assertBool "equal" (alphaEq (fromDeBruijn db2) ld2)
    assertBool "equal" (alphaEq (fromDeBruijn db3) ld3)
    assertBool "not equal" (Prelude.not $ alphaEq (fromDeBruijn (toDeBruijn true)) false)

sx :: Lambda String
sx = Abs "x" x
sy :: Lambda String
sy = Abs "y" y
sxy :: Lambda String
sxy = App x y

sub1 :: Subst String
sub1 = Subst "x" sx
sub2 :: Subst String
sub2 = Subst "y" sxy
sub3 :: Subst String
sub3 = Subst "z" sxy

unit_cas = do
    show (cas x sub1) @?= "\\x.x"
    show (cas true sub1) @?= "\\x.\\y.x"
    show (cas true sub2) @?= "\\a.\\y.a"
    show (cas ld3 sub3) @?= "\\a.\\b.a (b (x y))"