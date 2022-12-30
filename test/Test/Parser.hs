module Test.Parser where
    
 import Parser

 import Lambda
 import Text.Megaparsec
 import Text.Megaparsec.Char
 import Debug.Trace
 import Test.Tasty
 import Test.Tasty.HUnit

 varx :: Lambda String
 varx = Var "x"
 vary :: Lambda String
 vary = Var "y" 
 varz :: Lambda String
 varz = Var "z"
 t = Abs "w" (App (App (Var "w") (Var "w")) (Var "w"))
 term = App (Abs "x" (Var "z")) (App t t)
 
 unit_parse_lambda = do
     parseMaybe parseLambda "xx" @?= Just (Var "xx")
     parseMaybe parseLambda "λx.λy.x" @?= Just true
     parseMaybe parseLambda "λx.λy.y" @?= Just false
     parseMaybe parseLambda "(λx.z) ((λw.w w w) (λw.w w w))" @?= Just term
     parseMaybe parseLambda "λp.λq.p q p" @?= Just Lambda.and
     parseMaybe parseLambda "λp.λq.p p q" @?= Just Lambda.or
     parseMaybe parseLambda "λn.λf.λx.f (n f x)" @?= Just successor
     parseMaybe parseLambda  "λf.λx.x" @?= Just zero
     parseMaybe parseLambda "λf.λx.f x" @?= Just one 
     parseMaybe parseLambda "λf.λx.f (f x)" @?= Just two
     parseMaybe parseLambda "λx.λy.x (y z)" @?= Just ((Abs "x" $ Abs "y" $ App varx (App vary varz)))
     parseMaybe parseLambda "λf .x" @?= Nothing
     parseMaybe parseLambda "(λf.xx.) (x y)" @?= Nothing
     parseMaybe parseLambda "λf.λx.x1!" @?= Nothing

 unitTests :: [TestTree]
 unitTests =
    [ testCase "parse lambda" unit_parse_lambda
    ]     