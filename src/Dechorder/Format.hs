module Dechorder.Format where

import           Dechorder.Base

chordLookup :: [Key] -> String
-- C
chordLookup [C, E, G]      = "Chord C"
chordLookup [C, Eb, G]     = "Chord Cm"
chordLookup [C, E, G, B]   = "Chord Cmaj7"
chordLookup [C, Eb, G, Bb] = "Chord Cm7"
chordLookup [C, E, G, Bb]  = "Chord C7"
-- D
chordLookup [D, Gb, A]     = "Chord D"
chordLookup [D, F, A]      = "Chord Dm"
chordLookup [Db, D, Gb, A] = "Chord Dmaj7"
chordLookup [C, D, F, A]   = "Chord Dm7"
chordLookup [C, D, Gb, A]  = "Chord D7"
-- E
chordLookup [C, F, A]      = "Chord F"
chordLookup [D, G, B]      = "Chord G"
chordLookup [E, Ab, B]     = "Chord E"
chordLookup [E, G, B]      = "Chord Em"
chordLookup [E, A, B]      = "Chord Esus4"
chordLookup others         = show others
