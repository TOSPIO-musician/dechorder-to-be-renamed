module Dechorder.Analyze where

import           Data.Array                    as A
import           Data.Complex
import           Data.Foldable
import           Data.Function
import           Data.Vector                   as V
import           Debug.Trace
import           Dechorder.Type
import           Dechorder.Util
import qualified Numeric.Transform.Fourier.DFT as DFT

dft :: SampleChunk -> SampleChunkF
dft chunk = arrayToVector $ DFT.dft $ vectorToArray chunk


keepHalf :: SampleChunk -> SampleChunk
keepHalf chunk = V.take (V.length chunk `div` 2) chunk

analyze :: SamplingParams -> SampleChunk -> [(Frequency, Amplitude)]
analyze SamplingParams{..} chunk = let
  maxPos = maxIndexBy orderFunc $ keepHalf $ dft chunk
  freq = fromIntegral maxPos / duration
  in [(freq, amp2 $ chunk V.! maxPos)]
  where
    amp2 (a :+ b) = a * a + b * b
    amp = sqrt . amp2
    orderFunc = compare `on` amp2

