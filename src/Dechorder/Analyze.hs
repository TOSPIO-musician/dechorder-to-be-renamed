module Dechorder.Analyze where

import           Control.Arrow
import           Data.Array                    as A
import           Data.Complex
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Vector                   as V
import           Debug.Trace
import           Dechorder.Base
import           Dechorder.Record
import           Dechorder.Util
import qualified Numeric.Transform.Fourier.DFT as DFT

data AnalysisOptions = AnalysisOptions { samplingParams :: SamplingParams
                                       , range          :: (Frequency, Frequency)
                                       , maxNotes       :: Int
                                       , magFilter :: Amplitude -> Amplitude -> Bool
                                       }

defaultAnalysisOptions :: AnalysisOptions
defaultAnalysisOptions =
  AnalysisOptions { samplingParams = defaultSamplingParams
                  , range = (40, 20000)
                  , maxNotes = 4
                  , magFilter = \maxAmp amp -> amp >= maxAmp / 3
                  }

dft :: SampleChunk -> SampleChunkF
dft chunk = arrayToVector $ DFT.dft $ vectorToArray chunk

keepHalf :: SampleChunk -> SampleChunk
keepHalf chunk = V.take (V.length chunk `div` 2) chunk

keepHalfF :: SampleChunkF -> SampleChunkF
keepHalfF = keepHalf

toMagnitudeChunk :: SampleChunk -> MagnitudeChunk
toMagnitudeChunk = V.map magnitude

analyze :: AnalysisOptions -> SampleChunk -> [(Frequency, Amplitude)]
analyze AnalysisOptions{..} chunk = let
  freqDist = dft chunk
  halfFreqDist = keepHalfF freqDist
  magChunk = toMagnitudeChunk halfFreqDist
  freqChunk = V.imap (\idx val -> (fromIntegral idx / duration samplingParams, val)) magChunk
  boundedFreqChunk = V.takeWhile ((<= snd range) . fst) $ V.dropWhile ((< fst range) . fst) freqChunk
  in
  findDominant maxNotes boundedFreqChunk
  where
    findDominant maxCount boundedFreqChunk = let
      maxAmp = snd $ V.maximumBy (compare `on` snd) boundedFreqChunk
      threshold = maxAmp / 3
      filteredIndices = V.findIndices (magFilter maxAmp . snd) boundedFreqChunk
      filteredChunk = V.map (boundedFreqChunk V.!) filteredIndices
      in if V.length filteredIndices > maxCount
         then []
         else sortOn snd $ V.toList filteredChunk

