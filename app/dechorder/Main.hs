module Main where

import           Data.List
import           Dechorder

main :: IO ()
main = do
  s <- record Nothing
  let analyzeResult = analyze defaultAnalysisOptions s
  putStrLn $ chordLookup analyzeResult
