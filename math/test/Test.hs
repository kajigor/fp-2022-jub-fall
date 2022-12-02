import Test.Tasty

import qualified Test.Integral
import qualified Test.Expr
import qualified Test.Parser

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Integrals" Test.Integral.props
                , testGroup "Exprs" Test.Expr.props
                , testGroup "Parser" Test.Parser.props
                ])