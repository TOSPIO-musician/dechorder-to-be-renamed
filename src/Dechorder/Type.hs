module Dechorder.Type where

import           Data.Vector
import           Data.Complex

type SampleRate = Int
type Duration = Float

data SamplingParams = SamplingParams { sampleRate :: SampleRate
                                     , duration :: Duration
                                     }

type Frequency = Float
type Amplitude = Float

type SampleChunk = Vector (Complex Float)
type SampleChunkF = Vector (Complex Float)
