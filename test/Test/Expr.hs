{-# LANGUAGE ImplicitParams #-}

module Test.Expr where

import Test.Tasty.HUnit (Assertion, (@?=))
import Test.HUnit.Approx ((@?~))

import Expr

shouldBeResult :: Either ArithmeticError Double
               -> Either ArithmeticError Double
               ->  Assertion
shouldBeResult result exp =
  let ?epsilon = 0.000001 in
  case (result, exp) of
    (Right r, Right e) -> r @?~ e
    (Left r, Left e) -> r @?= e
    x -> fail $ "The result is not what we expected: " ++ show x

--unit_genExpr :: IO ()
--unit_genExpr = do
--    let results = [Right 13, Right (-42), Right 0.5
--                  , Left DivisionByZero, Left LogOfZero, Left LogOfNegativeNumber]
--    mapM_ check results
--  where
--    check :: Either ArithmeticError Double -> IO ()
--    check result =
--      mapM_ (uncurry shouldBeResult) [ (eval e, result) | e <- take 100 $ generateExprByResult result]