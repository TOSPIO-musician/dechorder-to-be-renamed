module Dechorder.Base where

import           Data.Vector
import           Data.Complex

type SampleRate = Int
type Duration = Double

type Frequency = Double
type Amplitude = Double

type SampleChunk = Vector Double
type SampleChunkF = Vector (Complex Double)
type MagnitudeChunk = Vector Amplitude

data Key = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
         deriving (Enum, Ord, Eq, Show)
