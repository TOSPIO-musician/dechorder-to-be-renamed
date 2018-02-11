module Dechorder.Transform where

import Data.Array
import qualified Numeric.Transform.Fourier.DFT as DFT
import Dechorder.Type

dft :: SampleChunk -> SampleChunkF
dft = DFT.dft
