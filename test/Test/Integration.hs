{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Integration where

import Test.Tasty.HUnit (Assertion, assertBool)
import Integration (evalIntegralMidPoint, evalIntegralTrapezoid, evalIntegralSimpson)

is_within :: Either String Double -> Double -> Double -> Bool
is_within (Left _) _ _ = False
is_within (Right a) b eps = abs (a - b) < eps


unit_evalIntegralMidPoint :: Assertion
unit_evalIntegralMidPoint = do
    assertBool "wrong answer on case 1" (is_within (evalIntegralMidPoint (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-3) 4.25 1e-3)
    assertBool "wrong answer on case 2" (is_within (evalIntegralMidPoint (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-5) 4.25 1e-5)
    assertBool "wrong answer on case 3" (is_within (evalIntegralMidPoint (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-8) 4.25 1e-8)
    assertBool "wrong answer on case 4" (is_within (evalIntegralMidPoint (\x -> cos (x / 2) * x) 2.0 5.0 1e-6) (-2.74694618) (1e-6))
    assertBool "wrong answer on case 5" (is_within (evalIntegralMidPoint (\x -> x*x*x*x*x-7*x*x*x*x+4*x*x*x-20*x*x+17*x+3) (-13) 20 1e-5) 4927909.8 1e-5)
    assertBool "wrong answer on case 6" (is_within (evalIntegralMidPoint (\x -> exp (3 * x / 5) + log (x / 4) / x) 13 25 1e-5) 5444295.60205442067009422450 1e-5)

unit_evalIntegralTrapezoid :: Assertion
unit_evalIntegralTrapezoid = do
    assertBool "wrong answer on case 1" (is_within (evalIntegralTrapezoid (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-3) 4.25 1e-3)
    assertBool "wrong answer on case 2" (is_within (evalIntegralTrapezoid (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-5) 4.25 1e-5)
    assertBool "wrong answer on case 3" (is_within (evalIntegralTrapezoid (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-8) 4.25 1e-8)
    assertBool "wrong answer on case 4" (is_within (evalIntegralTrapezoid (\x -> cos (x / 2) * x) 2.0 5.0 1e-6) (-2.74694618) (1e-6))
    assertBool "wrong answer on case 5" (is_within (evalIntegralTrapezoid (\x -> x*x*x*x*x-7*x*x*x*x+4*x*x*x-20*x*x+17*x+3) (-13) 20 1e-5) 4927909.8 1e-5)
    assertBool "wrong answer on case 6" (is_within (evalIntegralTrapezoid (\x -> exp (3 * x / 5) + log (x / 4) / x) 13 25 1e-5) 5444295.60205442067009422450 1e-5)

unit_evalIntegralSimpson :: Assertion
unit_evalIntegralSimpson = do
    assertBool "wrong answer on case 1" (is_within (evalIntegralSimpson (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-3) 4.25 1e-3)
    assertBool "wrong answer on case 2" (is_within (evalIntegralSimpson (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-5) 4.25 1e-5)
    assertBool "wrong answer on case 3" (is_within (evalIntegralSimpson (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-8) 4.25 1e-8)
    assertBool "wrong answer on case 4" (is_within (evalIntegralSimpson (\x -> cos (x / 2) * x) 2.0 5.0 1e-6) (-2.74694618) (1e-6))
    assertBool "wrong answer on case 5" (is_within (evalIntegralSimpson (\x -> x*x*x*x*x-7*x*x*x*x+4*x*x*x-20*x*x+17*x+3) (-13) 20 1e-5) 4927909.8 1e-5)
    assertBool "wrong answer on case 6" (is_within (evalIntegralSimpson (\x -> exp (3 * x / 5) + log (x / 4) / x) 13 25 1e-5) 5444295.60205442067009422450 1e-5)