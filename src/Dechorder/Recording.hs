module Dechorder.Recording where

import Data.Array
import Sound.Pulse.Simple
import Dechorder.Type

type SampleRate = Int
type Duration = Float

record :: SampleRate
       -> Duration
       -> IO (Array Int Float)
record sr dur = do
  s <- simpleNew
       Nothing  -- Server name
       "dechorder"  -- Client name
       Record  -- Play or Record
       Nothing  -- Name of sink or source
       "Dechorder sound sampler"  -- Description of client
       ( SampleSpec  -- The only SampleSpec constructor
         (F32  LittleEndian)
         sr
         1  -- Channels
       )  -- SampleSpec
       Nothing  -- Label channels
       Nothing  -- Buffer size, etc
  d <- simpleRead s (round $ (fromIntegral sr) * dur)
  simpleFree s
  return $ listArray (0, length d) d
