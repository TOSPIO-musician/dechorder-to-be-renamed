module Dechorder.Util where

import Data.Complex
import Data.Array
import Dechorder.Type

toSampleChunk :: [Float] -> SampleChunk
toSampleChunk l = listArray (lowerBound, upperBound) complexL
  where
    complexL = map (:+ 0) l
    lowerBound = 0
    upperBound = length l - 1
