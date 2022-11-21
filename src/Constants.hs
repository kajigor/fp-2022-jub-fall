module Constants where

import Lambda
import Reductions

-- true ≡ λx.λy.x
true :: Lambda 
true = Abs "x" (Abs "y" (Var "x"))

-- false ≡ λx.λy.y
false :: Lambda 
false = Abs "x" (Abs "y" (Var "y"))

-- and' ≡ λp.λq.p q p
and' :: Lambda 
and' = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- zero ≡ λf.λx.x
zero :: Lambda 
zero = Abs "f" (Abs "x" (Var "x"))

-- one ≡ λf.λx.f x
one :: Lambda 
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

-- two ≡ λf.λx.f (f x)
two :: Lambda 
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

--  four ≡ λf.λx.f (f (f (f x)))
four :: Lambda 
four = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add :: Lambda 
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor :: Lambda 
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- add' ≡ λm.λn.m successor n
add' :: Lambda 
add' = Abs "m" (Abs "n" (App (App (Var "m") successor) (Var "n")))

-- mult' ≡ λm.λn.m (add n) 0
mult' :: Lambda 
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- mult ≡ λm.λn.λf.m (n f)
mult :: Lambda 
mult = Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f")))))

x :: Lambda 
x = Var "x"

y :: Lambda 
y = Var "y"

z :: Lambda 
z = Var "z"

f :: Lambda 
f = Var "f"

xy :: Lambda 
xy = App x y

lxy :: Lambda 
lxy = Abs "x" y

sxy :: Subst 
sxy = Subst "x" y

sxz :: Subst 
sxz = Subst "x" z

lzx :: Lambda 
lzx = App (Var "z") (Var "x")

lxx :: Lambda 
lxx = Abs "x" x

lxlyz :: Lambda 
lxlyz = Abs "x" (Abs "y" z)

xyz :: Lambda 
xyz = App (App x y) z

mfnfx :: Lambda 
mfnfx = App (App (Var "m") (Var "f")) (App (App (Var "n") f) x)

twotimestwo :: Lambda 
twotimestwo = App (App mult two) two

twotimestwo' :: Lambda 
twotimestwo' = App (App mult' two) two

onetimesone' :: Lambda 
onetimesone' = App (App mult' one) one
