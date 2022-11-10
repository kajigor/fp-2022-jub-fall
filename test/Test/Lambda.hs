module Test.Lambda where
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Lambda

var1 :: Lambda String
var1 = Var "1"

var2 :: Lambda String
var2 = Var "2"

varX :: Lambda String
varX = Var "x"
varY :: Lambda String
varY = Var "y"

app1 :: Lambda String
app1 = App varX var1

lam1 :: Lambda String
lam1 = Abs "x" app1

unit_beautifulShow = do
  show true @?= "\\x.\\y.x"
  show false @?= "\\x.\\y.y"
  show ifThenElse @?= "\\p.\\a.\\b.p a b"
  show one @?= "\\f.\\x.f x"
  show four @?= "\\f.\\x.f (f (f (f x)))"
  show successor @?= "\\n.\\f.\\x.f (n f x)"
  show add' @?= "\\m.\\n.m (\\n.\\f.\\x.f (n f x)) n"
  show app1 @?= "x 1"
  show lam1 @?= "\\x.x 1"

trueDB = AbsDB (AbsDB (VarDB 2))
falseDB = AbsDB (AbsDB (VarDB 1))
ifThenElseDB = AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 2)) (VarDB 1))))
oneDB = AbsDB (AbsDB (AppDB (VarDB 2) (VarDB 1)))
fourDB = AbsDB (AbsDB (AppDB (VarDB 2) (AppDB (VarDB 2) (AppDB (VarDB 2) (AppDB (VarDB 2) (VarDB 1))))))

unit_showDeBrujn = do
  show trueDB @?= "\\ \\ 2"
  show falseDB @?= "\\ \\ 1"
  show ifThenElseDB @?= "\\ \\ \\ 3 2 1"
  show oneDB @?= "\\ \\ 2 1"
  show fourDB @?= "\\ \\ 2 (2 (2 (2 1)))"

unit_funcToDeBrujn = do
  toDeBruijn true @?= trueDB
  toDeBruijn false @?= falseDB
  toDeBruijn ifThenElse @?= ifThenElseDB
  toDeBruijn one @?= oneDB
  toDeBruijn four @?= fourDB
