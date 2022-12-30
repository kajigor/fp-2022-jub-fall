module Test.Reductions_pb where
    import Reductions
    import Lambda
    import Debug.Trace
    import Text.Megaparsec
    import Test.PB_tests
    import Hedgehog
    import qualified Hedgehog.Gen as Gen
    import qualified Hedgehog.Range as Range
    import Test.Tasty
    import Test.Tasty.Hedgehog

    getLast :: [a] -> a
    getLast [x] = x
    getLast (x : xs) = getLast xs
    getLast [] = error "reduction list was empty but not Nothing!"

    testReduction :: Lambda String -> Strategy -> Bool
    testReduction term strat = case reduct_list of 
                            Nothing -> True -- if not reduced we can't check it (mb it was limit) 
                            (Just res) -> alphaEq (Lambda.eval strat term) (getLast res)
                            where 
                                reduct_list = reductionList strat term  [term]

    prop_reduce_normal :: Property
    prop_reduce_normal = property $ do
        rnd_term <- forAll $ genLambda
        assert ((testReduction rnd_term NormalOrder) == True)

    prop_reduce_applicative :: Property
    prop_reduce_applicative = property $ do
        rnd_term <- forAll $ genLambda
        assert ((testReduction rnd_term ApplicativeOrder) == True)
    
    prop_reduce_byName :: Property
    prop_reduce_byName = property $ do
        rnd_term <- forAll $ genLambda
        assert ((testReduction rnd_term CallByName) == True)
    
    prop_reduce_byValue :: Property
    prop_reduce_byValue = property $ do
        rnd_term <- forAll $ genLambda
        assert ((testReduction rnd_term CallByValue) == True)


    
    props :: [TestTree]
    props =
        [ testProperty "reduction Normal Order" prop_reduce_normal
        , testProperty "reduction Applicative Order" prop_reduce_applicative
        , testProperty "reductio Call By Name" prop_reduce_byName
        , testProperty "reduction Call By Value" prop_reduce_byValue
        ]
