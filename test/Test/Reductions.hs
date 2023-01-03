module Test.Reductions where
    import Test.Tasty
    import Test.Tasty.HUnit

    import Reductions
    import Parser
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Lambda
    import Data.Maybe




    varx :: Lambda String
    varx = Var "x"
    vary :: Lambda String
    vary = Var "y" 
    varz :: Lambda String
    varz = Var "z"
    t = Abs "w" (App (App (Var "w") (Var "w")) (Var "w"))
    term_norm_only = App (Abs "x" (Var "z")) (App t t)
    term_norm_only_bad_show = "(λx.z) ((λw.w w w) (λw.w w w) (λw.w w w))"
    k = (Abs "x" (Abs "y" (Var "x")))
    i = (Abs "x" (Var "x"))
    s = (Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))))

    -- (λx.x x) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
    term_eval = App (Abs "x" (App (Var "x") (Var "x"))) (App (App (App k i) s) s)

    -- ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z))) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
    term_eval_ByValue = App (Abs "x" (App (Var "x") (Var "x"))) (App (App (Abs "y" (Abs "x" (Var "x"))) s) s)

    -- ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z))) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
    term_eval_ByName = App (App (App (App k i) s) s) (App (App (App k i) s) s)

    -- ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z))) ((λx.λy.x) (λx.x) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
    term_eval_Normal= App (App (App (App k i) s) s) (App (App (App k i) s) s)

    -- (λx.x x) ((λy.(λx.x)) (λx.λy.λz.x z (y z)) (λx.λy.λz.x z (y z)))
    term_eval_Applicative =  App (Abs "x" (App (Var "x") (Var "x"))) (App (App (Abs "y" (Abs "x" (Var "x"))) s) s)


    unit_reduce :: Assertion
    unit_reduce = do
        (alphaEq (fromJust (reduceOnce NormalOrder term_norm_only)) varz) @?= True
        (alphaEq (fromJust (reduceOnce CallByValue (App (Abs "x" $ Var "x") $ Var "y"))) vary) @?= True
        (alphaEq (fromJust (reduceOnce CallByValue term_eval)) term_eval_ByValue) @?= True
        (alphaEq (fromJust (reduceOnce CallByName term_eval)) term_eval_ByName) @?= True
        (alphaEq (fromJust (reduceOnce NormalOrder term_eval)) term_eval_Normal) @?= True
        (alphaEq (fromJust (reduceOnce ApplicativeOrder term_eval)) term_eval_Applicative) @?= True

        show (fromJust (reduceOnce ApplicativeOrder term_norm_only)) @?= term_norm_only_bad_show -- No end of this reduction
        show (fromJust (reduceOnce CallByValue term_norm_only)) @?= term_norm_only_bad_show -- No end of this reduction
        isNothing (reductionList ApplicativeOrder term_norm_only []) @?= True -- Return Nothing if no reduction
        isNothing (reductionList CallByValue term_norm_only []) @?= True -- Return Nothing if no reduction

        (show (eval ApplicativeOrder (fromJust (parseMaybe parseLambda "(λm.λn.λf.m (n f)) (λf.λn.f n) (λf.λn.f n)")))) @?= "λf.λa.f a"
        length (fromJust (reductionList ApplicativeOrder (fromJust (parseMaybe parseLambda "(λx.x) y")) [])) @?= 1

    unitTests :: [TestTree]
    unitTests =
        [ testCase "reduce" unit_reduce 
        ]