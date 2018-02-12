module Dechorder.Base where

import           Data.Vector
import           Data.Complex

type SampleRate = Int
type Duration = Float

type Frequency = Float
type Amplitude = Float

type SampleChunk = Vector (Complex Float)
type SampleChunkF = Vector (Complex Float)
type MagnitudeChunk = Vector Amplitude
