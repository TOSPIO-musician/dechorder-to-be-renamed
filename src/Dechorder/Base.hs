module Dechorder.Base where

import           Data.Vector
import           Data.Complex

type SampleRate = Int
type Duration = Float

type Frequency = Float
type Amplitude = Float

type SampleChunk = Vector Float
type SampleChunkF = Vector (Complex Float)
type MagnitudeChunk = Vector Amplitude

data Key = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
         deriving (Enum, Ord, Eq, Show)
