module Main where

import           Dechorder

samplingParams = SamplingParams { sampleRate = 22000
                                , duration = 0.5
                                }

main :: IO ()
main = do
  s <- record samplingParams
  let (freq, _):_ = analyze samplingParams s
  print $ freqKeyLookup freq
