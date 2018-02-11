module Main where

import           Dechorder

main :: IO ()
main = do
  s <- record 44100 0.1
  print s
