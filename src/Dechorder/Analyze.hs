module Dechorder.Analyze where

import           Control.Arrow
import           Data.Array                    as A
import           Data.Array.Conversion
import           Data.Complex
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Vector                   as V
import           Data.Vector.Conversion
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
                  , range = (40, 2000)
                  , maxNotes = 4
                  , magFilter = \maxAmp amp -> amp >= maxAmp / 4
                  }

dft :: SampleChunkF -> SampleChunkF
dft chunk = toVector $ DFT.dft $ toArray chunk

keepHalf :: SampleChunkF -> SampleChunkF
keepHalf chunk = V.take (V.length chunk `div` 2) chunk

keepHalfF :: SampleChunkF -> SampleChunkF
keepHalfF = keepHalf

toMagnitudeChunk :: SampleChunkF -> MagnitudeChunk
toMagnitudeChunk = V.map magnitude

analyzeF :: AnalysisOptions -> SampleChunkF -> [(Frequency, Amplitude)]
analyzeF AnalysisOptions{..} chunkF = let
  freqDist = dft chunkF
  halfFreqDist = keepHalfF freqDist
  magChunk = toMagnitudeChunk halfFreqDist
  freqChunk = V.imap (\idx val -> (fromIntegral idx / duration samplingParams, val)) magChunk
  boundedFreqChunk = V.takeWhile ((<= snd range) . fst) $ V.dropWhile ((< fst range) . fst) freqChunk
  in
  findDominant maxNotes boundedFreqChunk
  where
    findDominant maxCount boundedFreqChunk = let
      maxAmp = snd $ V.maximumBy (compare `on` snd) boundedFreqChunk
      filteredIndices = V.findIndices (magFilter maxAmp . snd) boundedFreqChunk
      filteredChunk = V.map (boundedFreqChunk V.!) filteredIndices
      in if V.length filteredIndices > maxCount
         then sortOn snd $ V.toList filteredChunk
         else sortOn snd $ V.toList filteredChunk

analyze :: AnalysisOptions -> SampleChunk -> [(Frequency, Amplitude)]
analyze options = analyzeF options . complexify
