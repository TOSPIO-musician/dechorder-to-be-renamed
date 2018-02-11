{-# LANGUAGE ImplicitParams #-}

module MetaTest where

import           Test.HUnit.Approx
import           Test.Tasty
import           Test.Tasty.HUnit
import           Util

tests :: TestTree
tests = testGroup "Tests for test"
  [ testCase "sinSeq" $ let ?epsilon = 0.001 in
      [ 0, 0.5, sqrt 3 / 2, 1, sqrt 3 / 2, 0.5
      , 0, -0.5, -sqrt 3 / 2, -1, -sqrt 3 / 2, -0.5
      ] @+~? take 12 (sinSeq (pi / 6))
  ]
