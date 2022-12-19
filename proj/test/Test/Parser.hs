
module Test.Parser where
    
import Test.Tasty
import Test.Tasty.HUnit

import Parser
import Lambda
import Test.Lambda

term1 = (App (Abs "x" (App (Var "t") (Var "x"))) (Abs "y" (Var "y")))

term2 = (App (App (Var "t") (Var "x")) (Abs "y" (Var "y")))

term3 = (App (Var "x") (App (Var "y") (App (Var "z") (Var "a"))))

term4 = (App (App (App (Var "z") (Var "a")) (Var "y")) (Var "x"))

term5 = (App (Var "x") (App (Var "y") (App (Abs "z" (Var "z")) (Var "a"))))

term6 = (App (Var "x") (App (App (Abs "z" (Var "z")) (Var "a")) (Var "y")))


unit_parse_string :: Assertion
unit_parse_string = do
    (runParser exprParser (show true)) @?= Just ("", true)
    (runParser exprParser (show add)) @?= Just ("", add)
    (runParser exprParser (show mult)) @?= Just ("", mult)
    (runParser exprParser (show or')) @?= Just ("", or')
    (runParser exprParser (show term1)) @?= Just ("", term1)
    (runParser exprParser (show term2)) @?= Just ("", term2)
    (runParser exprParser (show term3)) @?= Just ("", term3)
    (runParser exprParser (show term4)) @?= Just ("", term4)
    (runParser exprParser (show term5)) @?= Just ("", term5)

props :: [TestTree]
props = 
  [testCase "unit_parse_string" unit_parse_string]