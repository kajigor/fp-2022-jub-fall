module Test.Lambda where

import qualified Data.Set as Set
import Lambda
import Test.Tasty
import Test.Tasty.HUnit
import Data.Function

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

unit_show :: IO ()
unit_show = do
  show x @?= "x"
  show xy @?= "x y"
  show lxy @?= "\\x.y"
  show Lambda.true @?= "\\x.\\y.x"
  show Lambda.and @?= "\\p.\\q.p q p"
  show Lambda.two @?= "\\f.\\x.f (f x)"
  show Lambda.mult' @?= "\\m.\\n.m ((\\m.\\n.\\f.\\x.m f (n f x)) n) (\\f.\\x.x)"

unit_freeVariables :: IO ()
unit_freeVariables = do
  freeVariables x @?= Set.fromList ["x"]
  freeVariables xy @?= Set.fromList ["x", "y"]
  freeVariables lxy @?= Set.fromList ["y"]
  freeVariables Lambda.true @?= Set.empty
  freeVariables Lambda.and @?= Set.empty
  freeVariables Lambda.two @?= Set.empty
  freeVariables Lambda.mult' @?= Set.empty

unit_fresh :: IO ()
unit_fresh = do
  take 33 (candidates :: [String]) @?= ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "ba", "bb", "bc", "bd", "be", "bf", "bg"]

sxy :: Subst [Char]
sxy = Subst "x" y

sxz :: Subst [Char]
sxz = Subst "x" z

lzx :: Lambda [Char]
lzx = App (Var "z") (Var "x")

db1 :: Lambda String
db1 = Abs "x" (Abs "y" (Var "x"))

db2 :: Lambda String
db2 = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

db3 :: Lambda String
db3 = Abs "z" (App (Abs "y" (App (Var "y") (Abs "x" (Var "x")))) (Abs "x" (App (Var "z") (Var "x"))))

db1' :: Lambda String
db1' = Abs "y" (Abs "z" (Var "y"))

db1'' :: Lambda String
db1'' = Abs "y" (Abs "z" (Var "z"))

db2' :: Lambda String
db2' = Abs "a" (Abs "b" (Abs "c" (App (App (Var "a") (Var "c")) (App (Var "b") (Var "c")))))

db4 :: Lambda String
db4 = (Abs "x" (Var "y"))

twotimestwo :: Lambda [Char]
twotimestwo = App (App Lambda.mult Lambda.two) Lambda.two

twotimestwo' :: Lambda [Char]
twotimestwo' = App (App Lambda.mult' Lambda.two) Lambda.two

onetimesone' :: Lambda [Char]
onetimesone' = App (App Lambda.mult' Lambda.one) Lambda.one

unit_toDeBruijn :: IO ()
unit_toDeBruijn = do
  toDeBruijn db1 @?= AbsDB (AbsDB (VarDB 2))
  toDeBruijn db1' @?= AbsDB (AbsDB (VarDB 2))
  toDeBruijn db1'' @?= AbsDB (AbsDB (VarDB 1))
  toDeBruijn db2 @?= AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (VarDB 2) (VarDB 1))))) -- "\\1. \\2. \\3. 3 1 (2 1)"
  toDeBruijn db2' @?= AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (VarDB 2) (VarDB 1))))) -- "\\1. \\2. \\3. 3 1 (2 1)"
  toDeBruijn db3 @?= AbsDB (AppDB (AbsDB (AppDB (VarDB 1) (AbsDB (VarDB 1)))) (AbsDB (AppDB (VarDB 2) (VarDB 1)))) -- "\\ (\\ 1 (\\ 1)) (\\ 2 1)"
  toDeBruijn db4 @?= AbsDB (VarDB 2)

back1 :: Lambda String
back1 = fromDeBruijn $ toDeBruijn db1
back2 :: Lambda String
back2 = fromDeBruijn $ toDeBruijn db2
back3 :: Lambda String
back3 = fromDeBruijn $ toDeBruijn db3
back4 :: Lambda String
back4 = fromDeBruijn $ toDeBruijn db4

unit_fromDeBruijn :: IO ()
unit_fromDeBruijn = do
  toDeBruijn back1 @?= toDeBruijn db1
  toDeBruijn back2 @?= toDeBruijn db2
  toDeBruijn back3 @?= toDeBruijn db3
  toDeBruijn back4 @?= toDeBruijn db4

unit_alpha :: IO ()
unit_alpha = do
  alphaEq db1 db1' @?= True
  alphaEq db1 db1'' @?= False
  alphaEq db1 db2' @?= False
  alphaEq db2 db2' @?= True

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
  eval NormalOrder Lambda.successor @?= Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))) --  "\\n.\\f.\\x.f (n f x)"
  eval NormalOrder Lambda.mult @?= Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))) -- "\\m.\\n.\\f.m (n f)"
  eval NormalOrder Lambda.mult' @?= Abs "m" (Abs "n" (App (App (Var "m") (Abs "a" (Abs "f" (Abs "x" nfafx)))) fxx)) -- "\\m.\\n.m (\\a.\\f.\\x.n f (a f x)) (\\f.\\x.x)"
  eval NormalOrder twotimestwo @?= Lambda.four
  eval NormalOrder twotimestwo' @?= Lambda.four

alphaCmp :: (Ord a, Show a, HasCallStack) => Lambda a -> Lambda a -> Assertion
alphaCmp = (@?=) `on` toDeBruijn

unit_ao :: IO ()
unit_ao = do
  eval ApplicativeOrder Lambda.mult `alphaCmp` eval NormalOrder Lambda.mult
  eval ApplicativeOrder Lambda.mult' `alphaCmp` eval NormalOrder Lambda.mult'
  eval ApplicativeOrder onetimesone' `alphaCmp` eval NormalOrder onetimesone'
  eval ApplicativeOrder twotimestwo `alphaCmp` eval NormalOrder twotimestwo