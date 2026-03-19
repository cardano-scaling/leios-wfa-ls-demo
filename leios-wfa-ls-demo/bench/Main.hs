module Main (main) where

import qualified Bench.Certificate as Cert
import qualified Bench.NonPersistentVote as NPV
import qualified Bench.PersistentVote as PV
import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = do
  certBenches <- Cert.benchmarks
  defaultMain
    [ bgroup "pv" PV.benchmarks
    , bgroup "npv" NPV.benchmarks
    , bgroup "cert" certBenches
    ]
