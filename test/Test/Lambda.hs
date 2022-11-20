module Test.Lambda where

import Lambda
import Test.Tasty.HUnit

unit_show_lambda :: IO ()
unit_show_lambda = do
  show Lambda.true @?= "\\x.\\y.x"
  show Lambda.false @?= "\\x.\\y.y"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.or @?= "\\p.\\q.p p q"
  show Lambda.not @?= "\\p.p (" ++ show Lambda.false ++ ") (" ++ show Lambda.true ++ ")"
  show Lambda.ifThenElse @?= "\\p.\\a.\\b.p a b"
  show Lambda.zero @?= "\\f.\\x.x"
  show Lambda.one @?= "\\f.\\x.f x"
  show Lambda.three @?= "\\f.\\x.f (f (f x))"
  show Lambda.add @?= "\\m.\\n.\\f.\\x.m f (n f x)"
  show Lambda.successor @?= "\\n.\\f.\\x.f (n f x)"
  show Lambda.mult @?= "\\m.\\n.\\f.m (n f)"

unit_show_debruijn :: IO ()
unit_show_debruijn = do
  show Lambda.trueDB @?= "\\ \\ 1"
  show Lambda.falseDB @?= "\\ \\ 0"
  show Lambda.andDB @?= "\\ \\ 1 0 1"
  show Lambda.orDB @?= "\\ \\ 1 1 0"
  show Lambda.notDB @?= "\\ 0 (\\ \\ 0) (\\ \\ 1)"
  show Lambda.ifThenElseDB @?= "\\ \\ \\ 2 1 0"
  show Lambda.zeroDB @?= "\\ \\ 0"
  show Lambda.oneDB @?= "\\ \\ 1 0"
  show Lambda.threeDB @?= "\\ \\ 1 (1 (1 0))"
  show Lambda.addDB @?= "\\ \\ \\ \\ 3 1 (2 1 0)"
  show Lambda.successorDB @?= "\\ \\ \\ 1 (2 1 0)"
  show Lambda.multDB @?= "\\ \\ \\ 2 (1 0)"

t1 = Var "x"

t2 = App (Var "x") (Var "y")

t3 = App (Var "x") (Var "x")

t4 = Abs "a" (Var "x")

t5 = App (Abs "a" (Var "x")) (Abs "b" (Var "x"))

t6 = App (Abs "a" (Var "x")) (Abs "b" (Var "y"))

t7 = App (Abs "a" (Var "a")) (Abs "b" (Var "a"))


unit_to_debruijn :: IO ()
unit_to_debruijn = do
  toDeBruijn Lambda.true @?= Lambda.trueDB
  toDeBruijn Lambda.false @?= Lambda.falseDB
  toDeBruijn Lambda.and @?= Lambda.andDB
  toDeBruijn Lambda.or @?= Lambda.orDB
  toDeBruijn Lambda.not @?= Lambda.notDB
  toDeBruijn Lambda.ifThenElse @?= Lambda.ifThenElseDB
  toDeBruijn Lambda.zero @?= Lambda.zeroDB
  toDeBruijn Lambda.one @?= Lambda.oneDB
  toDeBruijn Lambda.three @?= Lambda.threeDB
  toDeBruijn Lambda.add @?= Lambda.addDB
  toDeBruijn Lambda.successor @?= Lambda.successorDB
  toDeBruijn Lambda.mult @?= Lambda.multDB
  toDeBruijn t1 @?= VarDB 0
  toDeBruijn t2 @?= AppDB (VarDB 0) (VarDB 1)
  toDeBruijn t3 @?= AppDB (VarDB 0) (VarDB 0)
  toDeBruijn t4 @?= AbsDB (VarDB 1)
  toDeBruijn t5 @?= AppDB (AbsDB (VarDB 1)) (AbsDB (VarDB 1))
  toDeBruijn t6 @?= AppDB (AbsDB (VarDB 1)) (AbsDB (VarDB 2))
  toDeBruijn t7 @?= AppDB (AbsDB (VarDB 0)) (AbsDB (VarDB 1))

unit_from_debruijn :: IO ()
unit_from_debruijn = do
  fromDeBruijn trueDB `alphaEq` true @?= True
  fromDeBruijn Lambda.falseDB `alphaEq` Lambda.false @?= True
  fromDeBruijn Lambda.andDB `alphaEq` Lambda.and @?= True
  fromDeBruijn Lambda.orDB `alphaEq` Lambda.or @?= True
  fromDeBruijn Lambda.notDB `alphaEq` Lambda.not @?= True
  fromDeBruijn Lambda.ifThenElseDB `alphaEq` Lambda.ifThenElse @?= True
  fromDeBruijn Lambda.zeroDB `alphaEq` Lambda.zero @?= True
  fromDeBruijn Lambda.oneDB `alphaEq` Lambda.one @?= True
  fromDeBruijn Lambda.threeDB `alphaEq` Lambda.three @?= True
  fromDeBruijn Lambda.addDB `alphaEq` Lambda.add @?= True
  fromDeBruijn Lambda.successorDB `alphaEq` Lambda.successor @?= True
  fromDeBruijn Lambda.multDB `alphaEq` Lambda.mult @?= True
  t5 `alphaEq` t6 @?= False
  t2 `alphaEq` t3 @?= False


unit_cas :: IO ()
unit_cas = do
  t1 `cas` (Subst "x" t2) @?= t2