module Test.Unit where

import Test.Tasty
import Test.Tasty.HUnit

alwaysSucceeds :: Assertion
alwaysSucceeds = do
  "Hello" @?= "Hello"

unitTests :: [TestTree]
unitTests =
  [testCase "Always succeeds" alwaysSucceeds]