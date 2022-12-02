module Test.Lambda where
    
 import Lambda 
 import Test.HUnit
 import Debug.Trace
 
 varx :: Lambda String
 varx = Var "x"
 vary :: Lambda String
 vary = Var "y" 
 varz :: Lambda String
 varz = Var "z"
 t = Abs "w" (App (App (Var "w") (Var "w")) (Var "w"))
 term = App (Abs "x" (Var "z")) (App t t)
 unit_show = do
     show term @?= "(λx.z) ((λw.w w w) (λw.w w w))"
     show varx @?= "x"
     show true @?= "λx.λy.x"
     show false @?= "λx.λy.y"
     show Lambda.and @?= "λp.λq.p q p"
     show Lambda.or @?= "λp.λq.p p q"
     show zero @?= "λf.λx.x"
     show one @?= "λf.λx.f x"
     show two @?= "λf.λx.f (f x)"
     show successor @?= "λn.λf.λx.f (n f x)"
     show (Abs "x" $ Abs "y" $ App varx (App vary varz))  @?= "λx.λy.x (y z)"

 deBruijn1 :: DeBruijn
 deBruijn1 = VarDB 0

 deBruijn2 :: DeBruijn
 deBruijn2 = AbsDB $ VarDB 0

 deBruijn3 :: DeBruijn
 deBruijn3 = AppDB (AbsDB $ VarDB 0) (VarDB 1)

 unit_show_DeBruijn = do
     show deBruijn1 @?= "0"
     show deBruijn2 @?= "λ 0"
     show deBruijn3 @?= "(λ 0) 1"

 unit_DeBruijn = do
     show (toDeBruijn varx) @?= "1"
     show (toDeBruijn true) @?= "λ λ 2"
     show (toDeBruijn false) @?= "λ λ 1"
     show (toDeBruijn Lambda.and) @?= "λ λ 2 1 2"
     show (toDeBruijn Lambda.or) @?= "λ λ 2 2 1"
     show (toDeBruijn successor) @?= "λ λ λ 2 (3 2 1)"
     show (toDeBruijn (Abs "x" $ Abs "y" $ App varx (App vary varz)))  @?= "λ λ 2 (1 3)"

 unit_fromDeBruijn = do
     show (fromDeBruijn (toDeBruijn true)) @?= "λa.λb.a"
     show (fromDeBruijn (toDeBruijn false)) @?= "λa.λb.b"
     show (fromDeBruijn (VarDB 1)) @?= "a"
     show (fromDeBruijn (AbsDB $ AbsDB $ AppDB (VarDB 2) (AppDB (VarDB 1) (VarDB 3))))  @?= "λa.λb.a (b c)"

 
 unit_alphaEq = do
     assertBool "equal" (Prelude.not $ alphaEq (App varx vary) (App varx varx))
     assertBool "not equal" (alphaEq vary vary)
     assertBool "not equal" (alphaEq (fromDeBruijn (toDeBruijn true)) true)
     assertBool "not equal" (alphaEq (Abs 0 (Abs 1 (App (App (Var 0) (Var 1)) (Var 0)))) Lambda.and)
     assertBool "equal" (Prelude.not $ alphaEq two one)

 freeExpr :: Lambda String
 freeExpr = Abs "x" $ Var "y"

 unit_cas = do
     show (cas freeExpr (Subst "z" (Var "string"))) @?= "λx.y"
     show (cas varx (Subst "x" (Abs "x" varx))) @?= "λx.x"
     show (cas false (Subst "x" (Abs "x" varx))) @?= "λx.λy.y"
     show (cas (Abs "x" $ Abs "y" $ App varx (App vary varz)) (Subst "a" (Abs "x" (App varx vary)))) @?= "λx.λz.x (z z)"


 instance Eq a => Eq (Lambda a) where
     (App a b) == (App c d) = a == c && b == d
     (Abs a b) == (Abs c d) = a == c && b == d
     (Var a) == (Var b) = a == b
     _ == _ = False
 

 unit_eval = do
     eval CallByValue (App (Abs "x" $ Var "x") $ Var "y") @?= Var "y"
     eval CallByValue (App (App true $ Var "a") $ Var "y'") @?= Var "a"
     
     eval CallByName (App (Abs "x" $ Var "x") $ Var "y") @?= Var "y"
     eval CallByName (App (App true $ Var "a") $ Var "y'") @?= Var "a"
    
     eval NormalOrder (App (Abs "x" $ Var "x") $ Var "y") @?= Var "y"
     eval NormalOrder (App (App true $ Var "a") $ Var "y'") @?= Var "a"
    
     eval ApplicativeOrder (App (Abs "x" $ Var "x") $ Var "y") @?= Var "y"
     eval ApplicativeOrder (App (App true $ Var "a") $ Var "y'") @?= Var "a"

     -- eval ApplicativeOrder term @?= Var "z" -- should be endlees
     eval NormalOrder term @?= Var "z"
