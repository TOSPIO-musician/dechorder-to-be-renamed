module Dechorder.Util where

import           Data.Array
import           Data.Array.ST
import           Data.Complex
import qualified Data.Vector      as V
import           Dechorder.Type
import           System.IO.Unsafe

toSampleChunk :: [Float] -> SampleChunk
toSampleChunk = V.fromList . map (:+ 0)

toSampleChunkF :: [Float] -> SampleChunkF
toSampleChunkF = toSampleChunk

vectorToArray :: V.Vector a -> Array Int a
vectorToArray v = runSTArray $ do
  a <- newArray_ (0, V.length v - 1)
  flip V.imapM_ v $ \idx val -> writeArray a idx val
  return a

arrayToVector :: Array Int a -> V.Vector a
arrayToVector a = let
  len = snd $ bounds a
  in
  V.generate len $ \idx -> a ! idx
