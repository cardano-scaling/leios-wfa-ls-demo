module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Cardano.Leios.BitMapPV as BitMapPV
import qualified Test.Cardano.Leios.LocalSortition as LocalSortition
import qualified Test.Cardano.Leios.NonIntegral as NonIntegral
import qualified Test.Cardano.Leios.Vote as Vote
import qualified Test.Cardano.Leios.WeightedFaitAccompli as WeightedFaitAccompli

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ WeightedFaitAccompli.tests
      , NonIntegral.tests
      , LocalSortition.tests
      , Vote.tests
      , BitMapPV.tests
      ]
