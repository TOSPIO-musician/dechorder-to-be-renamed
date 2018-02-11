module Dechorder.Recording where

import Sound.Pulse.Simple

type SampleRate = Int
type Duration = Float

record :: SampleRate
       -> Duration
       -> IO [Float]
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
  d <- simpleRead s $ (round $ (fromIntegral sr) * dur)
  simpleFree s
  return d
