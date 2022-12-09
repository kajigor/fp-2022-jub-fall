import Test.Tasty

import qualified Test.UnitTests
import qualified Test.PropertyBasedTests

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Unit" Test.UnitTests.unitTests
                , testGroup "Property based" Test.PropertyBasedTests.props
                ])