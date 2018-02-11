import Test.Tasty
import Test.Tasty.HUnit
import Dechorder.Internal

tests :: TestTree
tests = testGroup "Dechorder tests"
  [ testCase "DFT" $
    let samples = [0, 100, 200, 100, 0, 100, 200, 100, 0] :: [Float]
    in print $ dft $ toSampleChunk samples
  ]

main :: IO ()
main = defaultMain tests
