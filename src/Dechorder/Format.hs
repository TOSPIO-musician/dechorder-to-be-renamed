module Dechorder.Format where

import           Dechorder.Base

data Key = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
         deriving (Show, Eq)

freqRange :: (Frequency, Frequency)
freqRange = (220, 440)

normalize :: Frequency -> Frequency
normalize f
  | f < fst freqRange = normalize (f * 2)
  | f > snd freqRange = normalize (f / 2)
  | otherwise = f

freqKeyLookup :: Frequency -> Key
freqKeyLookup f = freqKeyLookup_ $ normalize f
  where
    freqKeyLookup_ f
      | f < 220 * 2**(1/24) = A
      | f < 220 * 2**(3/24) = Bb
      | f < 220 * 2**(5/24) = B
      | f < 220 * 2**(7/24) = C
      | f < 220 * 2**(9/24) = Db
      | f < 220 * 2**(11/24) = D
      | f < 220 * 2**(13/24) = Eb
      | f < 220 * 2**(15/24) = E
      | f < 220 * 2**(17/24) = F
      | f < 220 * 2**(19/24) = Gb
      | f < 220 * 2**(21/24) = G
      | f < 220 * 2**(23/24) = Ab
      | otherwise = A
