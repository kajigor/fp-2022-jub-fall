module Test.Integral where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Control.Exception (assert)
import Lib

sqr x = x * x

unit_integrals = do
    assertBool "should be similar" $ 
      abs (integrateWithStepsCount MiddleRectange sqr 0 1 100 - 1 / 3) < 0.001
    assertBool "should be similar" $ 
      abs (integrateWithStepsCount LinearApproximation sqr 0 1 100 - 1 / 3) < 0.001
    assertBool "should be similar" $ 
      abs (integrateWithStepsCount ParabolicApproximation sqr 0 1 100 - 1 / 3) < 0.001
    
    assertBool "should be similar" $ 
      abs (integrateWithStepsCount MiddleRectange sin 0 pi 10000 - 2) < 0.00001
    assertBool "should be similar" $ 
      abs (integrateWithStepsCount LinearApproximation sin 0 pi 10000 - 2) < 0.00001
    assertBool "should be similar" $ 
      abs (integrateWithStepsCount ParabolicApproximation sin 0 pi 10000 - 2) < 0.00001
    

    assertBool "error is too much" $
      abs (integrateWithError MiddleRectange sin 0 pi 0.001 - 2) < 0.001
    assertBool "error is too much" $
      abs (integrateWithError LinearApproximation sin 0 pi 0.001 - 2) < 0.001
    assertBool "error is too much" $
      abs (integrateWithError ParabolicApproximation sin 0 pi 0.001 - 2) < 0.001


    assertBool "error is too much" $
      abs (integrateWithError MiddleRectange sin 0 pi 0.00001 - 2) < 0.00001
    assertBool "error is too much" $
      abs (integrateWithError LinearApproximation sin 0 pi 0.00001 - 2) < 0.00001
    assertBool "error is too much" $
      abs (integrateWithError ParabolicApproximation sin 0 pi 0.00001 - 2) < 0.00001

    

