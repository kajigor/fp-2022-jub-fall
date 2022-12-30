module Test.PB_tests where 
    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog
   
    import Lambda

    genName :: Gen String
    genName = Gen.list (Range.singleton 1) Gen.alphaNum

    genLambda :: Gen (Lambda String)
    genLambda = 
        Gen.recursive
            Gen.choice
            [
                Lambda.Var <$> genName
            ]
            [
                genAbs,
                Gen.subterm2 genLambda genLambda App
            ]
        where
            genAbs = do
                x <- (Abs <$> genName)
                Gen.subterm genLambda x