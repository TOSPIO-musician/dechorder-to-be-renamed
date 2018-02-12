module Dechorder.Analyze where

import Control.Arrow
import Data.List
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

keepHalfF :: SampleChunkF -> SampleChunkF
keepHalfF = keepHalf

toMagnitudeChunk :: SampleChunk -> MagnitudeChunk
toMagnitudeChunk = V.map magnitude

findDominant :: Duration -> Int -> MagnitudeChunk -> [(Frequency, Amplitude)]
findDominant dur maxCount magChunk = let
  freqChunk = V.imap (\idx val -> (fromIntegral idx / dur, val)) magChunk
  maxAmp = V.maximum magChunk
  threshold = maxAmp / 1.5
  filteredIndices = V.findIndices ((> threshold) . snd) freqChunk
  filteredChunk = V.map (freqChunk V.!) filteredIndices
  in if V.length filteredIndices > 10
     then sortOn snd $ V.toList filteredChunk
     else sortOn snd $ V.toList filteredChunk

analyze :: SamplingParams -> SampleChunk -> [(Frequency, Amplitude)]
analyze SamplingParams{..} chunk =
  findDominant duration 11 (toMagnitudeChunk $ keepHalf $ dft chunk)
