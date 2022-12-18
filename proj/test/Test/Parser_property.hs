module Test.Parser_property where

import Parser
import Lambda
import Data.Maybe

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog


genLetter :: Gen String
genLetter = Gen.element (map (\x -> [x]) ['a'..'z'])

genLambda :: Gen (Lambda String)
genLambda =
  Gen.recursive
    Gen.choice
    [
        genVar
    ]
    [
        genLambda,
        genApp
    ]
  where
    genVar = Lambda.Var <$> genLetter
    genLambda = do
      lt <- (Lambda.Abs <$> genLetter)
      Gen.subterm genLambda lt
    genApp = do
      Gen.subterm2 genLambda genLambda (Lambda.App)


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