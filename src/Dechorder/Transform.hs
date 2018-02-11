module Dechorder.Transform where

import           Data.Array
import           Dechorder.Type
import qualified Numeric.Transform.Fourier.DFT as DFT

dft :: SampleChunk -> SampleChunkF
dft = DFT.dft
