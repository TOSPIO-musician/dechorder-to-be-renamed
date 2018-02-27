module Main where

import           Control.Concurrent
import qualified Control.Concurrent.BoundedChan        as BC
import           Control.Monad
import qualified Data.Vector                           as V
import           Dechorder
import qualified Dechorder.Internal                    as DI
import qualified Dechorder.Tool.SamplingVisualizer.GUI as SVGUI

sampleRate :: Int
sampleRate = 48000

duration :: Double
duration = 0.1

main :: IO ()
main = do
  window <- SVGUI.createWindow
  SVGUI.afterInitialized window $ do
    chan <- recordForever (Just sampleRate)
    forever $ do
      slice <- take (round $ fromIntegral sampleRate * duration) <$> BC.getChanContents chan
      let chunk = DI.toSampleChunk slice
          (slotWidth, result) = DI.chunkToFreqSlots
            DI.SamplingParams{ sampleRate = sampleRate, duration = duration }
            chunk
          plottingData = zip [0..] (V.toList result)
      SVGUI.updatePlottingArea window plottingData
  SVGUI.mainLoop
