module Test.Reductions where

import qualified Data.Set as Set
import Lambda
import Reductions
import Test.Tasty
import Test.Tasty.HUnit
import Data.Function

-- and' ≡ λp.λq.p q p
and' = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- zero ≡ λf.λx.x
zero :: Lambda [Char]
zero = Abs "f" (Abs "x" (Var "x"))

-- one ≡ λf.λx.f x
one :: Lambda [Char]
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

-- two ≡ λf.λx.f (f x)
two :: Lambda [Char]
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

--  four ≡ λf.λx.f (f (f (f x)))
four :: Lambda [Char]
four = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add :: Lambda [Char]
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor :: Lambda [Char]
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- mult ≡ λm.λn.λf.m (n f)
mult :: Lambda [Char]
mult = Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f")))))

-- mult' ≡ λm.λn.m (add n) 0
mult' :: Lambda [Char]
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))


x :: Lambda [Char]
x = Var "x"

y :: Lambda [Char]
y = Var "y"

z :: Lambda [Char]
z = Var "z"

xy :: Lambda [Char]
xy = App x y

lxy :: Lambda [Char]
lxy = Abs "x" y

sxy :: Subst [Char]
sxy = Subst "x" y

sxz :: Subst [Char]
sxz = Subst "x" z

lzx :: Lambda [Char]
lzx = App (Var "z") (Var "x")

twotimestwo :: Lambda [Char]
twotimestwo = App (App mult two) two

twotimestwo' :: Lambda [Char]
twotimestwo' = App (App mult' two) two

onetimesone' :: Lambda [Char]
onetimesone' = App (App mult' one) one


unit_cas :: IO ()
unit_cas = do
  cas x sxy @?= y
  cas xy sxy @?= App y y
  cas lxy sxy @?= lxy
  cas (Abs "z" y) sxy @?= Abs "z" (Var "y") 
  cas (Abs "z" x) sxy @?= Abs "z" (Var "y") 
  cas (Abs "z" x) sxz @?= Abs "a" (Var "z") 
  cas (Abs "y" x) sxz @?= Abs "y" (Var "z") 
  cas (Abs "z" lzx) sxz @?= Abs "a" (App (Var "a") (Var "z"))
  cas (Abs "f" (Abs "x" (Var "f"))) (Subst "a" (Var "f")) @?= Abs "b" (Abs "x" (Var "b"))

nfafx :: Lambda [Char]
nfafx = (App (App (Var "n") (Var "f")) (App (App (Var "a") (Var "f")) (Var "x")))

fxx :: Lambda [Char]
fxx = Abs "f" (Abs "x" (Var "x"))

unit_no :: IO ()
unit_no = do
  eval NormalOrder successor @?= Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))) --  "\\n.\\f.\\x.f (n f x)"
  eval NormalOrder mult @?= Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))) -- "\\m.\\n.\\f.m (n f)"
  eval NormalOrder mult' @?= Abs "m" (Abs "n" (App (App (Var "m") (Abs "a" (Abs "f" (Abs "x" nfafx)))) fxx)) -- "\\m.\\n.m (\\a.\\f.\\x.n f (a f x)) (\\f.\\x.x)"
  eval NormalOrder twotimestwo @?= four
  eval NormalOrder twotimestwo' @?= four

alphaCmp :: (Ord a, Show a, HasCallStack) => Lambda a -> Lambda a -> Assertion
alphaCmp = (@?=) `on` toDeBruijn

unit_ao :: IO ()
unit_ao = do
  eval ApplicativeOrder mult `alphaCmp` eval NormalOrder mult
  eval ApplicativeOrder mult' `alphaCmp` eval NormalOrder mult'
  eval ApplicativeOrder onetimesone' `alphaCmp` eval NormalOrder onetimesone'
  eval ApplicativeOrder twotimestwo `alphaCmp` eval NormalOrder twotimestwo