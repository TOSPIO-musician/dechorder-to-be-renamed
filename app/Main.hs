module Main where

import           Data.List
import           Dechorder

samplingParams = SamplingParams { sampleRate = 22000
                                , duration = 0.5
                                }

main :: IO ()
main = do
  s <- record samplingParams
  let analyzeResult = analyze samplingParams s
  print $ nub $ map (freqKeyLookup . fst) analyzeResult
