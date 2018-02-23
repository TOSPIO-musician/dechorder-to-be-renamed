module Tests.Dechorder where

import           Control.Monad
import qualified Data.Vector as V
import           Dechorder.Internal
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtil

keyFreqTable :: [[Frequency]]
keyFreqTable = [ [55.00, 110.00, 220.00, 440.00, 880.00, 1760.00]
               , [58.27, 116.54, 233.08, 466.16, 932.33, 1864.66]
               , [61.74, 123.47, 246.94, 493.88, 987.77, 1975.53]
               , [65.41, 130.81, 261.63, 523.25, 1046.50, 2093.00]
               , [69.30, 138.59, 277.18, 554.37, 1108.73, 2217.46]
               , [73.42, 146.83, 293.66, 587.33, 1174.66, 2349.32]
               , [77.78, 155.56, 311.13, 622.25, 1244.51, 2489.02]
               , [82.41, 164.81, 329.63, 659.26, 1318.51, 2637.02]
               , [87.31, 174.61, 349.23, 698.46, 1396.91, 2793.83]
               , [92.50, 185.00, 369.99, 739.99, 1479.98, 2959.96]
               , [98.00, 196.00, 392.00, 783.99, 1567.99, 3135.96]
               , [103.83, 207.65, 415.30, 830.61, 1661.22, 3322.44]
               ]

-- sin x + cos (2*x-pi/3)
piCycleChunk :: [Float]
piCycleChunk =
  [ 0.5, 1.2225624520175484, 1.5659328530262786, 1.4781476007338057, 1.055584979562807
  , 0.5, 3.7511058652552665e-2, -8.134535406638488e-2, 0.20448853110729415, 0.5
  ]

utilTests :: TestTree
utilTests = testGroup "Util tests"
  [ testCase "freqToKey" $ do
      forM_ (zip [A, Bb, B, C, Db, D, Eb, E, F, Gb, G, Ab] keyFreqTable) $ \(key, freqList) -> do
        forM_ freqList $ \freq -> do
          key @=? freqToKey freq
  ]

analyzeTests :: TestTree
analyzeTests = testGroup "Analyze tests"
  [ testCase "DFT for sine" $ do
      let samples = V.fromList $
            [ 0, 0.707, 1, 0.707, 0, -0.707, -1, -0.707
            , 0, 0.707, 1, 0.707, 0, -0.707, -1, -0.707
            ]
          expected = (0.5, V.fromList [0, 0, 1, 0, 0, 0, 0, 0])
          actual = chunkToFreqSlots SamplingParams{ sampleRate = 8
                                                  , duration = 2
                                                  } samples
      fst expected @=? fst actual
      let ?epsilon = 0.01 in V.toList (snd expected) @+~? V.toList (snd actual)
  ]

tests :: TestTree
tests = testGroup "Dechorder tests"
  [ utilTests
  , analyzeTests
  ]
