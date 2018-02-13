module Main where

import           Data.List
import           Dechorder

main :: IO ()
main = do
  let samplingParams = defaultSamplingParams
  s <- record samplingParams
  let analyzeResult = analyze defaultAnalysisOptions s
  print $ chordLookup $ nub $ sort $ map (freqKeyLookup . fst) analyzeResult
