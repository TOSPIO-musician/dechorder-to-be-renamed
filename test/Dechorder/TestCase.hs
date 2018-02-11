module Dechorder.TestCase where

import           Dechorder.Internal
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtil

tests :: TestTree
tests = testGroup "Dechorder tests"
  [ testCase "DFT" $
    let samples = [ 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
                  , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2, -0.5
                  , 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
                  , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2, -0.5
                  ] :: [Float]
    in printList $ dft $ toSampleChunk samples
  ]
