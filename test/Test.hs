import Test.Tasty

import qualified Test.ParsePB
import qualified Test.Lambda
import qualified Test.Parse
import qualified Test.Reductions


main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Lambda" Test.Lambda.unitTests
                , testGroup "Parse" Test.Parse.unitTests
                , testGroup "Reductions" Test.Reductions.unitTests
                , testGroup "Property-based" Test.ParsePB.props
                ])