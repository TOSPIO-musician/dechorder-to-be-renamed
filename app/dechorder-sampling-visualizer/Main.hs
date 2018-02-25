module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Dechorder
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Gtk
import qualified Graphics.UI.Gtk                        as G

createRenderableWindowWithCanvas :: G.DrawingArea
                                 -> Int
                                 -> Int
                                 -> IO G.Window
createRenderableWindowWithCanvas canvas windowWidth windowHeight = do
  window <- G.windowNew
  G.widgetSetSizeRequest window windowWidth windowHeight
  G.set window [G.containerChild G.:= canvas]
  return window

buildSpectrum :: [(Frequency, Amplitude)] -> Renderable ()
buildSpectrum freqDist = toRenderable $ execEC $ do
  layout_title .= "Spectrum Visualization"
  setColors [opaque blue, opaque red]
  plot (line "frequency" [freqDist])

updateSpectrum :: G.DrawingArea -> IO ()
updateSpectrum canvas = do
  let renderable = buildSpectrum [(0, 0), (1, 5), (2, 5)]
  G.postGUIAsync $ void $ updateCanvas renderable canvas
  threadDelay 2000000
  let renderable = buildSpectrum [(0, 0), (1, 20), (2, 5)]
  G.postGUIAsync $ void $ updateCanvas renderable canvas

main :: IO ()
main = do
  G.initGUI
  canvas <- G.drawingAreaNew
  window <- createRenderableWindowWithCanvas canvas 800 600
  G.on canvas G.exposeEvent $ do
    liftIO $ forkIO $ updateSpectrum canvas
    -- let renderable = toRenderable $ execEC $ do
    --       layout_title .= "Amplitude Modulation"
    --       setColors [opaque blue, opaque red]
    --       plot (line "am" [signal [0,(0.5)..400]])
    --       plot (points "am points" (signal [0,7..400]))
    return True
  G.widgetShowAll window

  -- Register event handler to properly quit the program
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False

  -- Main loop
  G.mainGUI
