{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Bench.NonPersistentVote (benchmarks) where

import Bench.Utils
  ( BenchEnv (..)
  , feasibleForLose
  , findLosingInput
  , findWinningInput
  , mkNPVCommittee
  , mkPoolId
  , mkPrivKey
  , mkPubKey
  )
import Cardano.Leios.LocalSortition (checkLeaderValueLeios)
import Cardano.Leios.Vote
  ( LeiosVote (..)
  , NonPersistentVote (..)
  , createNonPersistentVote
  , verifyLeiosVote
  )
import Cardano.Leios.WeightedFaitAccompli (CommitteeSelection (..))
import Control.Exception (evaluate)
import Control.Monad (void)
import Criterion.Main (Benchmark, bench, bgroup, perRunEnv)
import Data.Ratio ((%))
import Data.Word (Word16)

n2Values :: [Word16]
n2Values = [50, 70, 90, 110, 130, 150]

sigmaValues :: [Rational]
sigmaValues = [5 % 1000, 1 % 100, 5 % 100, 1 % 10, 1 % 4, 1 % 2]

showSigma :: Rational -> String
showSigma r = show (fromRational r :: Double)

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "non-persistent-vote" $
      [ bgroup ("n2=" ++ show n2) $
          [ bgroup ("sigma=" ++ showSigma sigma) $
              let poolId = mkPoolId 0
                  privKey = mkPrivKey poolId
                  pubKey = mkPubKey poolId
                  cs = mkNPVCommittee sigma n2 poolId pubKey
               in -- Creation (win): per-run setup finds a winning (nonce, eId, ebHash)
                  [ bench "create-win" $
                      perRunEnv
                        (BenchEnv . (\(n, e, h, _) -> (n, e, h)) <$> findWinningInput cs privKey)
                        ( \(BenchEnv (nonce, eId, ebHash)) ->
                            case createNonPersistentVote nonce cs privKey eId ebHash of
                              Left err -> error $ "create-win: unexpected loss: " ++ err
                              Right vote -> void $ evaluate (npvVoteSignature vote)
                        )
                  ]
                    -- Creation (lose): only when λ = σ×n2 < 5
                    ++ [ bench "create-lose" $
                          perRunEnv
                            (BenchEnv <$> findLosingInput cs privKey)
                            ( \(BenchEnv (nonce, eId, ebHash)) ->
                                void . evaluate $ createNonPersistentVote nonce cs privKey eId ebHash
                            )
                       | feasibleForLose sigma n2
                       ]
                    -- Verification: per-run setup finds a winning vote
                    ++ [ bench "verify" $
                          perRunEnv
                            (BenchEnv <$> findWinningInput cs privKey)
                            ( \(BenchEnv (nonce, eId, ebHash, vote)) ->
                                let cs' = cs{praosNonce = nonce}
                                 in void . evaluate $
                                      verifyLeiosVote cs' eId ebHash (LeiosNonPersistentVote vote)
                            )
                       , -- Isolated Taylor expansion: fresh VRF output per run
                         bench "sortition-check" $
                          perRunEnv
                            ( BenchEnv . npvEligibilitySignature . (\(_, _, _, v) -> v)
                                <$> findWinningInput cs privKey
                            )
                            ( \(BenchEnv vrfOut) ->
                                void . evaluate $
                                  checkLeaderValueLeios vrfOut sigma (fromIntegral @Word16 @Integer n2)
                            )
                       ]
          | sigma <- sigmaValues
          ]
      | n2 <- n2Values
      ]
  ]
