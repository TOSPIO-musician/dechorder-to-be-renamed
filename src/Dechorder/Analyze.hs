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
import qualified Data.Vector.Mutable           as MV
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
                  , range = (100, 2000)
                  , maxNotes = 4
                  , magFilter = \maxAmp amp -> amp >= maxAmp / 2
                  }

dft :: SampleChunkF -> SampleChunkF
dft chunk = toVector $ DFT.dft $ toArray chunk

keepHalf :: SampleChunkF -> SampleChunkF
keepHalf chunk = V.take (V.length chunk `div` 2) chunk

keepHalfF :: SampleChunkF -> SampleChunkF
keepHalfF = keepHalf

toMagnitudeChunk :: SampleChunkF -> MagnitudeChunk
toMagnitudeChunk = V.map magnitude

analyzeF :: AnalysisOptions -> SampleChunkF -> [Key]
analyzeF AnalysisOptions{..} chunkF = let
  freqDist = dft chunkF
  halfFreqDist = keepHalfF freqDist
  magChunk = toMagnitudeChunk halfFreqDist
  freqChunk = V.imap (\idx val -> (fromIntegral idx / duration samplingParams, val)) magChunk
  boundedFreqChunk = V.takeWhile ((<= snd range) . fst) $ V.dropWhile ((< fst range) . fst) freqChunk
  in
  findDominant maxNotes $ scatterToKeySlots boundedFreqChunk
  where
    scatterToKeySlots :: V.Vector (Frequency, Amplitude) -> V.Vector Frequency
    scatterToKeySlots freqChunk = V.create $ do
      slots <- MV.replicate 12 0
      flip V.mapM_ freqChunk $ \(freq, amp) -> do
        MV.modify slots (+amp) (keyToIndex $ freqToKey freq)
      return slots
    findDominant maxNotes keySlots = let
      maxAmp = V.maximum keySlots
      filteredIndices = V.findIndices (magFilter maxAmp) keySlots
      in if V.length filteredIndices > maxNotes
         then sort $ map indexToKey $ V.toList filteredIndices
         else sort $ map indexToKey $ V.toList filteredIndices

analyze :: AnalysisOptions -> SampleChunk -> [Key]
analyze options = analyzeF options . complexify
