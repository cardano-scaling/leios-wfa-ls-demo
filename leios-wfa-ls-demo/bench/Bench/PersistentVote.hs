{-# LANGUAGE DataKinds #-}

module Bench.PersistentVote (benchmarks) where

import Bench.Utils
  ( mkPoolId
  , mkPrivKey
  , mkPVCommittee
  , testEbHash
  , testElectionId
  )
import Cardano.Leios.Crypto (KeyRoleLeios (..), PrivateKeyLeios)
import Cardano.Leios.Vote
  ( LeiosVote (..)
  , PersistentVote (..)
  , createPersistentVote
  , verifyLeiosVote
  )
import Cardano.Leios.WeightedFaitAccompli (CommitteeSelection)
import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (void)
import Criterion.Main (Benchmark, bench, bgroup, env, whnf, whnfIO)

data PVSetup = PVSetup CommitteeSelection (PrivateKeyLeios 'Vote) PersistentVote

instance NFData PVSetup where
  rnf x = x `seq` ()

setupPV :: IO PVSetup
setupPV = do
  let poolId = mkPoolId 0
      privKey = mkPrivKey poolId
      cs = mkPVCommittee privKey
  case createPersistentVote cs privKey testElectionId testEbHash of
    Left err -> error $ "setupPV: " ++ err
    Right vote -> return $ PVSetup cs privKey vote

benchmarks :: [Benchmark]
benchmarks =
  [ env setupPV $ \pvSetup ->
      let PVSetup cs privKey vote = pvSetup
       in bgroup
            "persistent-vote"
            [ bench "create" $ whnfIO $
                case createPersistentVote cs privKey testElectionId testEbHash of
                  Left err -> error err
                  Right newVote -> void $ evaluate (pvVoteSignature newVote)
            , bench "verify" $
                whnf
                  (verifyLeiosVote cs testElectionId testEbHash)
                  (LeiosPersistentVote vote)
            ]
  ]
