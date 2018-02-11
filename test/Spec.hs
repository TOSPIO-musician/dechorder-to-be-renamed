import           Dechorder.Internal
import qualified Dechorder.TestCase as DechorderTC (tests)
import qualified MetaTest           (tests)
import           Test.Tasty
import           Test.Tasty.HUnit


allTests :: TestTree
allTests = testGroup "All tests" $
  [ MetaTest.tests
  , DechorderTC.tests
  ]

main :: IO ()
main = defaultMain allTests
