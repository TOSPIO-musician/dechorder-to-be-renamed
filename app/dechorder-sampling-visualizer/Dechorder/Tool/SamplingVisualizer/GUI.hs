module Dechorder.Tool.SamplingVisualizer.GUI where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Dechorder
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Gtk
import qualified Graphics.UI.Gtk                        as G

-- | The ADT that encapsulates the GTK window and drawing area
data Window =
  Window { gtkWindow      :: !G.Window  -- ^ The GTK window
         , gtkDrawingArea :: !G.DrawingArea  -- ^ The canvas where spectrums are plotted
         }

createRenderableWindowWithCanvas :: G.DrawingArea
                                 -> Int
                                 -> Int
                                 -> IO G.Window
createRenderableWindowWithCanvas canvas windowWidth windowHeight = do
  window <- G.windowNew
  G.widgetSetSizeRequest window windowWidth windowHeight
  G.set window [G.containerChild G.:= canvas]
  return window

-- | Create the main GUI window.
createWindow :: IO Window
createWindow = do
  G.initGUI
  canvas <- G.drawingAreaNew
  window <- createRenderableWindowWithCanvas canvas 800 600
  G.widgetShowAll window

  -- Register event handler to properly quit the program
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False

  return $ Window window canvas

updatePlottingArea :: Window -> [(Double, Double)] -> IO ()
updatePlottingArea Window{..} samples =
  G.postGUIAsync $ void $ updateCanvas (buildCanvas samples) gtkDrawingArea
  where
    buildCanvas :: [(Double, Double)] -> Renderable ()
    buildCanvas plottingData = toRenderable $ execEC $ do
      layout_title .= "Spectrum Visualization"
      setColors [opaque blue, opaque red]
      plot (line "frequency" [plottingData])

afterInitialized :: Window -> IO () -> IO ()
afterInitialized Window{..} callback =
  void $ gtkDrawingArea `G.on` G.exposeEvent $ do
    liftIO $ forkIO $ callback
    return True

mainLoop :: IO ()
mainLoop = G.mainGUI
