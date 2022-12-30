import Test.Tasty

import qualified Test.Lambda
import qualified Test.Parser
import qualified Test.Reductions
import qualified Test.Parser_pb
import qualified Test.Reductions_pb

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Parser" Test.Parser.unitTests
                , testGroup "Lambda" Test.Lambda.unitTests
                , testGroup "Reductions" Test.Reductions.unitTests
                , testGroup "Parser_property" Test.Parser_pb.props
                , testGroup "Reductions_property" Test.Reductions_pb.props
                ])