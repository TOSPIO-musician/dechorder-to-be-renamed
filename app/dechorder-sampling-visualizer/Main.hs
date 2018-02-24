module Main where

import           Dechorder
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Gtk
import           Graphics.UI.Gtk

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main :: IO ()
main = do
  let renderable = (toRenderable (execEC $ do
                                     layout_title .= "Amplitude Modulation"
                                     setColors [opaque blue, opaque red]
                                     plot (line "am" [signal [0,(0.5)..400]])
                                     plot (points "am points" (signal [0,7..400]))))
  renderableToWindow renderable 800 600
