import Test.Tasty
import Test.Tasty.Hspec

import qualified Test.List
import qualified Test.Expr

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "List" Test.List.props
                , testGroup "Expr" Test.Expr.props
                ])