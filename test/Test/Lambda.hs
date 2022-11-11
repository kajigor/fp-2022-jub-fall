module Test.Lambda where

import Lambda
import Test.Tasty.HUnit ((@?=), assertBool)

x :: Lambda String
x = Var "x"
y :: Lambda String
y = Var "y"

one :: Lambda Int
one = Var 1
two :: Lambda Int
two = Var 2

-- lmd ≡ λy.y (λx.x)
lmd :: Lambda String
lmd = Abs "y" $ App y (Abs "x" x)

-- lmd ≡ λ2.2 (λ1.1)
lmdi :: Lambda Int
lmdi = Abs 2 $ App Test.Lambda.two (Abs 1 Test.Lambda.one)


unit_show = do
  show x @?= "x"
  show y @?= "y"
  show Lambda.true @?= "\\x.\\y.x"
  show Lambda.false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show Lambda.not @?= "\\p.p (\\x.\\y.y) (\\x.\\y.x)"
  show Lambda.ifThenElse @?= "\\p.\\a.\\b.p a b"
  show Lambda.zero @?= "\\f.\\x.x"
  show Lambda.one @?= "\\f.\\x.f x"
  show Lambda.two @?= "\\f.\\x.f (f x)"
  show Lambda.three @?= "\\f.\\x.f (f (f x))"
  show Lambda.four @?= "\\f.\\x.f (f (f (f x)))"
  show Lambda.add @?= "\\m.\\n.\\f.\\x.m f (n f x)"
  show Lambda.successor @?= "\\n.\\f.\\x.f (n f x)"
  show Lambda.mult @?= "\\m.\\n.\\f.m (n f)"
  show lmd @?= "\\y.y (\\x.x)"
  
  show Test.Lambda.one @?= "1"
  show Test.Lambda.two @?= "2"
  show lmdi @?= "\\2.2 (\\1.1)"