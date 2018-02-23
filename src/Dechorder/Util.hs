module Dechorder.Util where

import           Data.Array
import           Data.Array.ST
import           Data.Complex
import qualified Data.Vector      as V
import           Dechorder.Base
import           System.IO.Unsafe

toSampleChunk :: [Double] -> SampleChunk
toSampleChunk = V.fromList

toSampleChunkF :: [Double] -> SampleChunkF
toSampleChunkF = complexify . toSampleChunk

complexify :: SampleChunk -> SampleChunkF
complexify = V.map (:+ 0)

{-# INLINE freqToKey #-}
freqToKey :: Frequency -> Key
freqToKey freq = let
  _12bt = 12 * (logBase 2 freq - logBase 2 440) + 0.5
  diff = _12bt - fromIntegral (floor (_12bt / 12)) * 12
  in
  if | diff < 0   -> error "Impossible"
     | diff < 1   -> A
     | diff < 2   -> Bb
     | diff < 3   -> B
     | diff < 4   -> C
     | diff < 5   -> Db
     | diff < 6   -> D
     | diff < 7   -> Eb
     | diff < 8   -> E
     | diff < 9   -> F
     | diff < 10  -> Gb
     | diff < 11  -> G
     | diff <= 12 -> Ab
     | otherwise  -> error "Impossible"
