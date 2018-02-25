module Dechorder.Record where

import           Control.Concurrent
import           Control.Concurrent.BoundedChan (BoundedChan)
import qualified Control.Concurrent.BoundedChan as BC
import           Control.Monad
import           Data.Array
import           Dechorder.Base
import           Dechorder.Util
import           Sound.Pulse.Simple


-- | Sampling parameters used when recording.
data SamplingParams = SamplingParams { sampleRate :: SampleRate
                                     , duration   :: Duration
                                     }

-- | The default sampling parameters when not specified.
defaultSamplingParams :: SamplingParams
defaultSamplingParams = SamplingParams { sampleRate = 48000
                                       , duration = 0.1
                                       }

newSimpleConnection :: SampleRate -> IO Simple
newSimpleConnection sampleRate =
  simpleNew
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

-- | Record with PulseAudio for a given period at the given sample rate, as is specified in the sampling parameters.
record :: Maybe SamplingParams
       -> IO SampleChunk
record Nothing = record $ Just defaultSamplingParams
record (Just SamplingParams{..}) = do
  s <- newSimpleConnection sampleRate
  d <- simpleRead s (round $ (fromIntegral sampleRate) * duration)
  simpleFree s
  return $ toSampleChunk d

-- | Record with PulseAudio forever, yielding a BoundedChan of samples.
recordForever :: Maybe SampleRate
              -> IO (BoundedChan Double)
recordForever Nothing = recordForever $ Just $ sampleRate defaultSamplingParams
recordForever (Just sampleRate) = do
  chan <- BC.newBoundedChan (sampleRate * 10)
  s <- newSimpleConnection sampleRate
  forkIO $ forever $ do
    d <- simpleRead s (round $ (fromIntegral sampleRate) * 0.1)
    mapM_ (BC.writeChan chan) d
  return chan
