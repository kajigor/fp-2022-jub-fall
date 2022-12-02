{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Parser where

import Test.Tasty.HUnit
import Test.Tasty

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog

import Parser
import Expr

unit_string_parser :: IO ()
unit_string_parser = do
    runParser (string "abc") "abcde" @?= Just ("de", "abc")
    runParser (string "abc") "abbde" @?= Nothing
    runParser (string "sin") "sin(0)" @?= Just ("(0)", "sin")

unit_expr_parser :: IO ()
unit_number_parser = do
    runParser number "12345 ABCDE" @?= Just (" ABCDE", 12345)
    runParser number "-12" @?= Just ("", -12)
    runParser number "-12.001" @?= Just ("", -12.001)
    runParser number "123.123" @?= Just ("", 123.123)
    runParser number "123.000123" @?= Just ("", 123.000123)
    runParser number "0.0001" @?= Just ("", 0.0001)

unit_number_parser :: IO ()
unit_expr_parser = do
    let x = Expr.Var "x"
    let y = Expr.Var "y"
    let z = Expr.Var "z"
    let minus = BinOp Minus
    let plus = BinOp Plus
    let mult = BinOp Mult
    let div' = BinOp Div
    let pow = BinOp Pow


    runParser exprParser "-3.14" @?= Just ("", Number (-3.14))
    runParser exprParser "x" @?= Just ("", x)
    runParser exprParser "-x" @?= Just ("", UnaryOp UnaryMinus x)
    runParser exprParser "x-y+z-x+y-z" @?= Just ("", x `minus` y `plus` z `minus` x `plus` y `minus` z)
    runParser exprParser "x*y/z*x" @?= Just ("", x `mult` y `div'` z `mult` x)
    runParser exprParser "1-variable" @?= Just ("", minus (Number 1) (Expr.Var "variable"))
    runParser exprParser "x*y+x/y" @?= Just ("", plus (mult x y) (div' x y))
    runParser exprParser "sin(1-x)" @?= Just ("", UnaryOp Sin $ minus (Number 1) x)
    runParser exprParser "cos(x^abs(y))" @?= Just ("", UnaryOp Cos $ BinOp Pow x $ UnaryOp Abs y)
    runParser exprParser "x+z*(-y)^((sin(z)))" @?=
        Just ("", plus x $ mult z $ BinOp Pow (UnaryOp UnaryMinus y) $ UnaryOp Sin z)
    runParser exprParser "sin(x^cos(y))*x" @?= Just("", UnaryOp Sin (x `pow` UnaryOp Cos y) `mult` x)

genExpr :: Int -> Gen Expr
genExpr depth = do
    a <- if depth == 1
    then Gen.int (Range.constant 0 1)
    else Gen.int (Range.constant 0 3)

    case a of
        0 -> Number <$> Gen.double (Range.constant (-100) 100)
        1 -> Expr.Var    <$> Gen.string (Range.constant 1 10) Gen.lower
        2 -> do
            l <- genExpr $ depth - 1
            r <- genExpr $ depth - 1
            op <- Gen.element [Plus, Minus, Mult, Div, Pow]
            return $ BinOp op l r
        3 -> do
            e <- genExpr $ depth - 1
            op <- Gen.element [UnaryMinus, Sin, Cos, Abs]
            return $ UnaryOp op e
        _ -> undefined


prop_parser :: Property
prop_parser = property $ do
    expr <- forAll $ genExpr 6
    case runParser exprParser (show expr) of
        Just ("", expr2) -> Hedgehog.assert $ approx expr expr2
        _ -> Hedgehog.assert False
    where
        approx :: Expr -> Expr -> Bool
        approx (Number a) (Number b) = abs (a - b) < 0.00001
        approx (Expr.Var x) (Expr.Var y) = x == y
        approx (BinOp op1 l1 r1) (BinOp op2 l2 r2) = op1 == op2 && approx l1 l2 && approx r1 r2
        approx (UnaryOp op1 e1) (UnaryOp op2 e2) = op1 == op2 && approx e1 e2
        approx _ _ = False

props :: [TestTree]
props =
  [ testCase "string parser" unit_string_parser
  , testCase "number parser" unit_number_parser
  , testCase "expr parser" unit_expr_parser
  , testProperty "expr parser via pb testing" prop_parser
  ]