import           Dechorder.Internal
import qualified Tests.Dechorder as DechorderTests (tests)
import qualified MetaTest           (tests)
import           Test.Tasty
import           Test.Tasty.HUnit


allTests :: TestTree
allTests = testGroup "All tests" $
  [ MetaTest.tests
  , DechorderTests.tests
  ]

main :: IO ()
main = defaultMain allTests
