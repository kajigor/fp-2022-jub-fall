import Test.Tasty

import qualified Test.List
import qualified Test.Expr
import qualified Test.Unit

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "List" Test.List.props
                , testGroup "Expr" Test.Expr.props
                , testGroup "Unit" Test.Unit.unitTests
                ])