module Test.Lambda where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)

import Lambda

person = ""

unit_showstring = do
  show true @?= "\\x.\\y.x"
  show false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show Lambda.not @?= "\\p.p (\\x.\\y.y) (\\x.\\y.x)"
  show ifThenElse @?= "\\p.\\a.\\b.p a b"
  show zero @?= "\\f.\\x.x"
  show one @?= "\\f.\\x.f x"
  show two @?= "\\f.\\x.f (f x)"
  show three @?= "\\f.\\x.f (f (f x))"
  show four @?= "\\f.\\x.f (f (f (f x)))"
  show add @?= "\\m.\\n.\\f.\\x.m f (n f x)"
  show successor @?= "\\n.\\f.\\x.f (n f x)"
  show add' @?= "\\m.\\n.m (\\n.\\f.\\x.f (n f x)) n"
  show mult @?= "\\m.\\n.m (n f)"
  show mult' @?= "\\m.\\n.m ((\\m.\\n.\\f.\\x.m f (n f x)) n) (\\f.\\x.x)"


multNum = Abs (1 :: Integer) (Abs (2 :: Integer) (App (Var (1 :: Integer)) (App (Var (2 :: Integer)) (Var (3 :: Integer)))))

unit_showany = do
  show multNum @?= "\\1.\\2.1 (2 3)"

db1 = AbsDB (AbsDB (VarDB 2))
db2 = AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (VarDB 2) (VarDB 1)))))
--db3 = AbsDB (AppDB (AppDB (AbsDB (VarDB 1)) (AbsDB (VarDB 1))) (AbsDB (AppDB (VarDB 2) (VarDB 1))))
db3 = AbsDB (AppDB (AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 1)))) (AbsDB (AppDB (VarDB 2) (VarDB 1))))

unit_showdb = do
  show db1 @?= "\\ \\ 2"
  show db2 @?= "\\ \\ \\ 3 1 (2 1)"
  show db3 @?= "\\ (\\ 1 (\\ 1)) (\\ 2 1)"
