import Test.Tasty

import qualified Test.Lambda
import qualified Test.Parse
import qualified Test.Reductions
import qualified Test.ParsePB
import qualified Test.ReductionsPB


main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Lambda" Test.Lambda.unitTests
                , testGroup "Parse" Test.Parse.unitTests
                , testGroup "Reductions" Test.Reductions.unitTests
                , testGroup "Property-based parse" Test.ParsePB.props
                , testGroup "Property-based reductions" Test.ReductionsPB.props
                ])