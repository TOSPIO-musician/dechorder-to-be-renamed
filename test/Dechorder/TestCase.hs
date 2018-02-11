module Dechorder.TestCase where

import           Data.Array
import           Dechorder.Internal
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtil

sineWaveSamplePoints :: [Float]
sineWaveSamplePoints =
  [ 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
  , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2, -0.5
  , 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
  , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2
  , 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
  , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2, -0.5
  , 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
  , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2
  ]

sineWaveSampleChunk :: SampleChunk
sineWaveSampleChunk = toSampleChunk sineWaveSamplePoints

sineWaveSamplingParams :: SamplingParams
sineWaveSamplingParams = SamplingParams { sampleRate = 24
                                        , duration = 2
                                        }

tests :: TestTree
tests = testGroup "Dechorder tests"
  [ testCase "DFT" $ do
      let maxFreq = analyze sineWaveSamplingParams sineWaveSampleChunk
      2.0 @=? fst ((analyze sineWaveSamplingParams sineWaveSampleChunk) !! 0)
  ]
