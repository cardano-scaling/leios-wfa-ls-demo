module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Cardano.Leios.NonIntegral as NonIntegral
import qualified Test.Cardano.Leios.WeightedFaitAccompli as WeightedFaitAccompli

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ WeightedFaitAccompli.tests
      , NonIntegral.tests
      ]
