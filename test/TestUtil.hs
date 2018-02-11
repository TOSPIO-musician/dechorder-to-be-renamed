module TestUtil where

import           Control.Monad
import           Test.HUnit.Approx
import           Test.Tasty.HUnit

printList :: (Traversable t, Show a) => t a -> IO ()
printList = void . traverse print

sin :: Double -> Double -> Double
sin f t = Prelude.sin (f * t)

cos :: Double -> Double -> Double
cos f t = Prelude.cos (f * t)

sinSeq :: Double -> [Double]
sinSeq f = map (Prelude.sin . (*f)) [0..]

cosSeq :: Double -> [Double]
cosSeq f  = map (Prelude.cos . (*f)) [0..]

(@+~?) :: (?epsilon :: Double) => [Double] -> [Double] -> Assertion
expected @+~? actual = sequence_ $ map (uncurry (@~?)) $ zip expected actual

(@+?~) :: (?epsilon :: Double) => [Double] -> [Double] -> Assertion
actual @+?~ expected = sequence_ $ map (uncurry (@~?)) $ zip actual expected
