module Test.Parse where

import qualified Data.Set as Set
import Lambda
import Constants
import Parse
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char

unit_listToApp :: IO()
unit_listToApp = do
  listToApp [x] @?= x
  listToApp [x, y] @?= xy
  listToApp [x, y, z] @?= xyz

unit_parseLambda :: IO ()
unit_parseLambda = do
  parseMaybe parseLambda (show x) @?= Just x
  parseMaybe parseLambda (show xy) @?= Just xy
  parseMaybe parseLambda (show lxx) @?= Just lxx
  parseMaybe parseLambda (show lxlyz) @?= Just lxlyz
  parseMaybe parseLambda (show zero) @?= Just zero
  parseMaybe parseLambda (show one) @?= Just one
  parseMaybe parseLambda (show two) @?= Just two
  parseMaybe parseLambda (show four) @?= Just four
  parseMaybe parseLambda (show mult) @?= Just mult
  parseMaybe parseLambda (show xyz) @?= Just xyz
  parseMaybe parseLambda (show mfnfx) @?= Just mfnfx
  parseMaybe parseLambda (show add) @?= Just add
  parseMaybe parseLambda (show mult') @?= Just mult'
  parseMaybe parseLambda "" @?= Nothing
  parseMaybe parseLambda "λn. λf. λx. f (n f x)" @?= Nothing
  parseMaybe parseLambda "λn_1.λf.λx.f (n_1 f x)" @?= Nothing
  parseMaybe parseLambda "λn.λf.λx.f (n f x)" @?= Just successor
  parseMaybe parseLambda "\\n.\\f.\\x.f (n f x)" @?= Just successor