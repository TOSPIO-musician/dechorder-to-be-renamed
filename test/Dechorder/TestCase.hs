module Dechorder.TestCase where

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

sineWaveAnalysisOptions :: AnalysisOptions
sineWaveAnalysisOptions = defaultAnalysisOptions{ samplingParams = sineWaveSamplingParams
                                                , range = (0, 100)
                                                }

-- sin x + cos (2*x-pi/3)
piCycleChunk :: [Float]
piCycleChunk =
  [ 0.5, 1.2225624520175484, 1.5659328530262786, 1.4781476007338057, 1.055584979562807
  , 0.5, 3.7511058652552665e-2, -8.134535406638488e-2, 0.20448853110729415, 0.5
  ]

tests :: TestTree
tests = testGroup "Dechorder tests"
  [ testCase "DFT" $ do
      let maxFreq = analyze sineWaveAnalysisOptions sineWaveSampleChunk
      2.0 @=? fst ((analyze sineWaveAnalysisOptions sineWaveSampleChunk) !! 0)
  ]
