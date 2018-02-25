module Dechorder.Analyze where

import           Control.Arrow
import           Data.Array                         as A
import           Data.Array.Conversion
import           Data.Complex
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Vector                        as V
import           Data.Vector.Conversion
import qualified Data.Vector.Mutable                as MV
import           Debug.Trace
import           Dechorder.Base
import           Dechorder.Record
import           Dechorder.Util
import qualified Numeric.Transform.Fourier.DFT      as DFT
import qualified Numeric.Transform.Fourier.FFTUtils as FFTUtils

data AnalysisOptions = AnalysisOptions { samplingParams :: SamplingParams
                                       , range          :: (Frequency, Frequency)
                                       , maxNotes       :: Int
                                       , magFilter :: Amplitude -> Amplitude -> Bool
                                       }

defaultAnalysisOptions :: AnalysisOptions
defaultAnalysisOptions =
  AnalysisOptions { samplingParams = defaultSamplingParams
                  , range = (100, 2000)
                  , maxNotes = 4
                  , magFilter = \maxAmp amp -> amp >= maxAmp / 2
                  }

dft :: SampleChunk -> SampleChunkF
dft = toVector . DFT.dft . toArray . complexify

rfft :: SampleChunk -> SampleChunk
rfft = toVector . FFTUtils.rfft_mag . toArray

chunkToFreqSlots :: SamplingParams -> SampleChunk -> (Double, MagnitudeChunk)
chunkToFreqSlots !params !chunk = let
  !dftResult = rfft chunk
  !length = V.length dftResult
  !reservedLength = length `div` 2
  !normalizedResult = V.map ((/ (fromIntegral length)) . (*2)) $ V.take reservedLength dftResult
  slotWidth = 1 / duration params
  in (slotWidth, normalizedResult)

keepHalf :: SampleChunkF -> SampleChunkF
keepHalf chunk = V.take (V.length chunk `div` 2) chunk

keepHalfF :: SampleChunkF -> SampleChunkF
keepHalfF = keepHalf

analyze :: AnalysisOptions -> SampleChunk -> [Key]
analyze AnalysisOptions{..} chunk = let
  (slotWidth, slotDist) = chunkToFreqSlots samplingParams chunk
  freqDist = V.zip (V.fromList [0, slotWidth..]) slotDist
  maxAmp = V.maximum slotDist
  minAmp = V.minimum slotDist
  threshold = minAmp + (maxAmp - minAmp) / 8
  boundedFreqChunk = V.takeWhile ((<= snd range) . fst) $ V.dropWhile ((< fst range) . fst) freqDist
  in
  findDominant maxNotes $ scatterToKeySlots boundedFreqChunk
  where
    scatterToKeySlots :: V.Vector (Frequency, Amplitude) -> V.Vector Frequency
    scatterToKeySlots freqChunk = V.create $ do
      slots <- MV.replicate 12 0
      flip V.mapM_ freqChunk $ \(freq, amp) -> do
        MV.modify slots (+amp) (fromEnum $ freqToKey freq)
      return slots
    findDominant maxNotes keySlots = let
      maxAmp = V.maximum keySlots
      filteredIndices = V.findIndices (magFilter maxAmp) keySlots
      in if V.length filteredIndices > maxNotes
         then sort $ map toEnum $ V.toList filteredIndices
         else sort $ map toEnum $ V.toList filteredIndices
