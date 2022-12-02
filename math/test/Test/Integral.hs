{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Integral where

import Test.Tasty.HUnit
import Lib

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

sqr :: Num a => a -> a
sqr x = x * x

unit_integrals :: IO ()
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


newtype Func = Func { f :: Double -> Double }

functionsList :: [Func]
functionsList = [Func sin,
                 Func cos,
                 Func id,
                 Func (** 0.5),
                 Func (** 100),
                 Func $ \x -> 0.5 * x + 0.5,
                 Func $ \x -> x * (1 - x) ** 2,
                 Func $ \x -> 1 / (1 + x * x)]


instance Show Func where
  show func =
    let vals = map (/10) $ take 10 [0..] in
    show $ zip vals $ map (f func) vals


genFunction :: Gen Func
genFunction =
  (\f' g' h' -> Func $ f f' . f g' . f h') <$>
  Gen.element functionsList <*>
  Gen.element functionsList <*>
  Gen.element functionsList


genError :: Gen Double
genError = Gen.double (Range.constant 0.001 1)

genBound :: Gen Double
genBound = Gen.double (Range.constant 0.0 1.0)

prop_error :: Property
prop_error = property $ do
  fun <- forAll genFunction
  let func = f fun
  err <- forAll genError
  left <- forAll genBound
  right <- forAll genBound
  let intLin = integrateWithError LinearApproximation func left right err
  let intRec = integrateWithError MiddleRectange func left right err
  let intPar = integrateWithError ParabolicApproximation func left right err
  Hedgehog.assert (checkAllSame 1 intLin intRec intPar)

  where
    checkAllSame :: Double -> Double -> Double -> Double -> Bool
    checkAllSame err a b c =
      abs(a - b) < 2 * err &&
      abs(a - c) < 2 * err &&
      abs(b - c) < 2 * err


props :: [TestTree]
props = 
  [ testProperty "Error is too big" prop_error
  , testCase "integrals" unit_integrals
  ]