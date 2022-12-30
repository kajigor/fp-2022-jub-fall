module Test.Parser_pb where
    import Parser
    import Lambda
    import Debug.Trace
    import Text.Megaparsec
    import Test.PB_tests
    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog


    testShown :: String -> Lambda String -> Bool
    testShown shw term = case parsed of 
                            Nothing -> False
                            (Just res) -> alphaEq res term
                            where 
                                parsed = parseMaybe parseLambda shw

    prop_base_parse :: Property
    prop_base_parse = property $ do
        rnd_term <- forAll $ genLambda
        let shw = show rnd_term
        assert ((testShown shw rnd_term ) == True)
    
    props :: [TestTree]
    props =
        [ testProperty "show x parse alpha eq" prop_base_parse 
        ]
