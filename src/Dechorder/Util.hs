module Dechorder.Util where

import           Data.Array
import           Data.Array.ST
import           Data.Complex
import qualified Data.Vector      as V
import           Dechorder.Base
import           System.IO.Unsafe

toSampleChunk :: [Float] -> SampleChunk
toSampleChunk = V.fromList

toSampleChunkF :: [Float] -> SampleChunkF
toSampleChunkF = complexify . toSampleChunk

complexify :: SampleChunk -> SampleChunkF
complexify = V.map (:+ 0)

{-# INLINE keyToIndex #-}
keyToIndex :: Key -> Int
keyToIndex C  = 0
keyToIndex Db = 1
keyToIndex D  = 2
keyToIndex Eb = 3
keyToIndex E  = 4
keyToIndex F  = 5
keyToIndex Gb = 6
keyToIndex G  = 7
keyToIndex Ab = 8
keyToIndex A  = 9
keyToIndex Bb = 10
keyToIndex B  = 11

{-# INLINE indexToKey #-}
indexToKey :: Int -> Key
indexToKey 0  = C
indexToKey 1  = Db
indexToKey 2  = D
indexToKey 3  = Eb
indexToKey 4  = E
indexToKey 5  = F
indexToKey 6  = Gb
indexToKey 7  = G
indexToKey 8  = Ab
indexToKey 9  = A
indexToKey 10 = Bb
indexToKey 11 = B
indexToKey _  = error "Impossible"

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
