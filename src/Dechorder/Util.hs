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
