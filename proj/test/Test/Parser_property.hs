module Test.Parser_property where

import Parser
import Lambda
import Data.Maybe

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.LambdaGenerator


parserShowIsId :: MonadTest m => (String -> Maybe (String, (Lambda String))) -> Lambda String -> m ()
parserShowIsId parser ast =
  case parser (show ast) of
    Just ("", r) -> ((alphaEq r ast) === True)
    _ -> failure

prop_myParserShow :: Property
prop_myParserShow = property $ do
  lambda <- forAll $ genLambda
  parserShowIsId (runParser exprParser) lambda

props :: [TestTree]
props =
  [ testProperty "`parser . printer == id`" prop_myParserShow
  ]