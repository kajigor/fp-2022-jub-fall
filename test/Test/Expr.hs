{-# LANGUAGE ImplicitParams #-}

module Test.Expr where

import Test.Tasty.HUnit (Assertion, (@?=))
import Test.HUnit.Approx ((@?~))

import Expr
    (ArithmeticError (..),
    Expr (..),
    eval,
    generateExprByResult,
    generateDivExpr,
    generateMultExpr,
    generateSumExpr,
    generateSubExpr,
    generateRootExpr,
    generatePowELogExpr,
    generateExprByError,
    generateExprByValue,
    getRemainderDouble,
    generateAdditionalValue)

shouldBeResult :: Either ArithmeticError Double
               -> Either ArithmeticError Double
               ->  Assertion
shouldBeResult result exp =
  let ?epsilon = 0.000001 in
  case (result, exp) of
    (Right r, Right e) -> r @?~ e
    (Left r, Left e) -> r @?= e
    x -> fail $ "The result is not what we expected: " ++ show x ++ ", expression: " ++ show exp

shouldBeDoubleResult :: Either ArithmeticError Double -> Double -> Assertion
shouldBeDoubleResult result e =
  let ?epsilon = 0.000001 in
    case result of
      Right r -> r @?~ e
      Left r -> fail $ "The result is not what we expected: " ++ show r ++ ", expected: " ++ show e

shouldBeErrorResult :: Either ArithmeticError Double -> ArithmeticError -> Assertion
shouldBeErrorResult result e =
  case result of
    Right r -> fail $ "The result is not what we expected: " ++ show r ++ ", expected: " ++ show e
    Left r -> r @?= e

unit_genExpr :: IO ()
unit_genExpr = do
    let results = [Right 13, Right (-42), Right 0.5
                  , Left DivisionByZero, Left LogOfZero, Left LogOfNegativeNumber]
    mapM_ check results
  where
    check :: Either ArithmeticError Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeResult) [ (eval e, result) | e <- take 100 $ generateExprByResult result 100]

unit_generateDivExpr :: IO()
unit_generateDivExpr = do
    let results = [13.0, -42.0, 0.5]
    mapM_ check results
  where
    check :: Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeDoubleResult) [ (eval e, result) | e <- [generateDivExpr result]]

unit_generateMultExpr :: IO()
unit_generateMultExpr = do
    let results = [13.0, -42.0, 0.5]
    mapM_ check results
  where
    check :: Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeDoubleResult) [ (eval e, result) | e <- [generateMultExpr result]]

unit_generateSubExpr :: IO()
unit_generateSubExpr = do
    let results = [13.0, -42.0, 0.5]
    mapM_ check results
  where
    check :: Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeDoubleResult) [ (eval e, result) | e <- [generateSubExpr result]]

unit_generateSumExpr :: IO()
unit_generateSumExpr = do
    let results = [13.0, -42.0, 0.5]
    mapM_ check results
  where
    check :: Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeDoubleResult) [ (eval e, result) | e <- [generateSumExpr result]]

unit_generateRootExpr :: IO()
unit_generateRootExpr = do
    let results = [13.0, -42.0, 0.5]
    mapM_ check results
  where
    check :: Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeDoubleResult) [ (eval e, result) | e <- [generateRootExpr result]]

unit_generatePowELogExpr :: IO()
unit_generatePowELogExpr = do
    let results = [13.0, -42.0, 0.5, -1]
    mapM_ check results
  where
    check :: Double -> IO ()
    check result =
      mapM_ (uncurry shouldBeDoubleResult) [ (eval e, result) | e <- [generatePowELogExpr result]]

unit_generateExprByError :: IO()
unit_generateExprByError = do
    let results = [DivisionByZero, LogOfZero, LogOfNegativeNumber, DegreeOfZero, RootNegativeBase]
    mapM_ check results
  where
    check :: ArithmeticError -> IO ()
    check result =
      mapM_ (uncurry shouldBeErrorResult) [ (eval e, result) | e <- [generateExprByError result (Val 1)]]

unit_generateAdditionalValue :: IO()
unit_generateAdditionalValue = do
  generateAdditionalValue 1 @?= 2
  generateAdditionalValue 2 @?= -2
  generateAdditionalValue 3 @?= 6
  generateAdditionalValue 14 @?= 2
  generateAdditionalValue 5 @?= 8
  generateAdditionalValue 6 @?= 1
  generateAdditionalValue 7 @?= 42
  generateAdditionalValue 18 @?= 2
  generateAdditionalValue 9 @?= 17
  generateAdditionalValue 30 @?= 100

unit_generateExprByValue :: IO()
unit_generateExprByValue = do 
  eval (generateExprByValue 1.1) @?= Right 1.1
  eval (generateExprByValue 2.2) @?= Right 2.2
  eval (generateExprByValue 3.3) @?= Right 3.3
  eval (generateExprByValue 4.4) @?= Right 4.4
  eval (generateExprByValue 5.5) @?= Right 5.5
  -- eval (generateExprByValue 6.6) @?~ Right 6.6
  eval (generateExprByValue 7.7) @?= Right 7.7
  -- eval (generateExprByValue 42.42) @?~ Right 42.42

unit_getRemainderDouble :: IO()
unit_getRemainderDouble = do
  getRemainderDouble 5.4 7 @?= 5
  getRemainderDouble 6.1 7 @?= 6
  getRemainderDouble 6.6 7 @?= 0
  getRemainderDouble 7.7 7 @?= 1