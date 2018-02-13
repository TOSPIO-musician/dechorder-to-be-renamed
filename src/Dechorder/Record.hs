module Dechorder.Record where

import           Data.Array
import           Dechorder.Base
import           Dechorder.Util
import           Sound.Pulse.Simple


data SamplingParams = SamplingParams { sampleRate :: SampleRate
                                     , duration :: Duration
                                     }

defaultSamplingParams :: SamplingParams
defaultSamplingParams = SamplingParams { sampleRate = 48000
                                       , duration = 0.2
                                       }

record :: SamplingParams
       -> IO SampleChunk
record SamplingParams{..} = do
  s <- simpleNew
       Nothing  -- Server name
       "dechorder"  -- Client name
       Record  -- Play or Record
       Nothing  -- Name of sink or source
       "Dechorder sound sampler"  -- Description of client
       ( SampleSpec  -- The only SampleSpec constructor
         (F32 LittleEndian)
         sampleRate
         1  -- Channels
       )  -- SampleSpec
       Nothing  -- Label channels
       Nothing  -- Buffer size, etc
  d <- simpleRead s (round $ (fromIntegral sampleRate) * duration)
  simpleFree s
  return $ toSampleChunk d
