module Main (main) where

import qualified Bench.NonPersistentVote as NPV
import qualified Bench.PersistentVote as PV
import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main =
  defaultMain
    [ bgroup "pv" PV.benchmarks
    , bgroup "npv" NPV.benchmarks
    ]
