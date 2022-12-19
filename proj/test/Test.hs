import Test.Tasty

import qualified Test.Lambda
import qualified Test.Parser
import qualified Test.Reductions
import qualified Test.Parser_property
import qualified Test.Reductions_property

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Parser" Test.Parser.props
                , testGroup "Lambda" Test.Lambda.props
                , testGroup "Reductions" Test.Reductions.props
                , testGroup "Parser_property" Test.Parser_property.props
                , testGroup "Reductions_property" Test.Reductions_property.props
                ])