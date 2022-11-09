module Test.Lambda where

import qualified Data.Set as Set
import Lambda
import Test.Tasty
import Test.Tasty.HUnit

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

twotimestwo :: Lambda [Char]
twotimestwo = App (App Lambda.mult Lambda.two) Lambda.two

twotimestwo' :: Lambda [Char]
twotimestwo' = App (App Lambda.mult' Lambda.two) Lambda.two

onetimesone' :: Lambda [Char]
onetimesone' = App (App Lambda.mult' Lambda.one) Lambda.one

unit_toDeBruijn :: IO ()
unit_toDeBruijn = do
  show (toDeBruijn db1) @?= "\\ \\ 2"
  show (toDeBruijn db1') @?= "\\ \\ 2"
  show (toDeBruijn db1'') @?= "\\ \\ 1"
  show (toDeBruijn db2) @?= "\\ \\ \\ 3 1 (2 1)"
  show (toDeBruijn db2') @?= "\\ \\ \\ 3 1 (2 1)"
  show (toDeBruijn db3) @?= "\\ (\\ 1 (\\ 1)) (\\ 2 1)"

back1 :: Lambda String
back1 = fromDeBruijn $ toDeBruijn db1
back2 :: Lambda String
back2 = fromDeBruijn $ toDeBruijn db2
back3 :: Lambda String
back3 = fromDeBruijn $ toDeBruijn db3

unit_fromDeBruijn :: IO ()
unit_fromDeBruijn = do
  show (toDeBruijn back1) @?= show (toDeBruijn db1)
  show (toDeBruijn back2) @?= show (toDeBruijn db2)
  show (toDeBruijn back3) @?= show (toDeBruijn db3)

unit_alpha :: IO ()
unit_alpha = do
  alphaEq db1 db1' @?= True
  alphaEq db1 db1'' @?= False
  alphaEq db1 db2' @?= False
  alphaEq db2 db2' @?= True

unit_cas :: IO ()
unit_cas = do
  show (cas x sxy) @?= "y"
  show (cas xy sxy) @?= "y y"
  show (cas lxy sxy) @?= "\\x.y"
  show (cas (Abs "z" y) sxy) @?= "\\z.y"
  show (cas (Abs "z" x) sxy) @?= "\\z.y"
  show (cas (Abs "z" x) sxz) @?= "\\a.z"
  show (cas (Abs "y" x) sxz) @?= "\\y.z"
  show (cas (Abs "z" lzx) sxz) @?= "\\a.a z"
  show (cas (Abs "f" (Abs "x" (Var "f"))) (Subst "a" (Var "f"))) @?= "\\b.\\x.b"

unit_no :: IO ()
unit_no = do
  show (eval NormalOrder Lambda.successor) @?= "\\n.\\f.\\x.f (n f x)"
  show (eval NormalOrder Lambda.mult) @?= "\\m.\\n.\\f.m (n f)"
  show (eval NormalOrder Lambda.mult') @?= "\\m.\\n.m (\\a.\\f.\\x.n f (a f x)) (\\f.\\x.x)"
  show (eval NormalOrder twotimestwo) @?= show Lambda.four
  show (eval NormalOrder twotimestwo') @?= show Lambda.four

unit_ao :: IO ()
unit_ao = do
  (show . toDeBruijn) (eval ApplicativeOrder Lambda.mult) @?= "\\ \\ \\ 3 (2 1)"
  (show . toDeBruijn) (eval ApplicativeOrder Lambda.mult') @?= "\\ \\ 2 (\\ \\ \\ 4 2 (3 2 1)) (\\ \\ 1)"
  (show . toDeBruijn) (eval ApplicativeOrder onetimesone') @?= (show . toDeBruijn) Lambda.one
  (show . toDeBruijn) (eval ApplicativeOrder twotimestwo) @?= (show . toDeBruijn) Lambda.four