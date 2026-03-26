module Bench.PersistentVote (benchmarks) where

import Bench.Utils (
  BenchEnv (..),
  mkPVCommittee,
  mkPoolId,
  mkPrivKey,
  randomBenchInputs,
 )
import Cardano.Leios.Crypto (PrivateKeyLeios)
import Cardano.Leios.Vote (
  LeiosVote (..),
  PersistentVote (..),
  createPersistentVote,
  verifyLeiosVote,
 )
import Cardano.Leios.WeightedFaitAccompli (CommitteeSelection)
import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (void)
import Criterion.Main (Benchmark, bench, bgroup, env, perRunEnv)

data PVSetup = PVSetup CommitteeSelection PrivateKeyLeios

instance NFData PVSetup where
  rnf x = x `seq` ()

setupPV :: IO PVSetup
setupPV = do
  let poolId = mkPoolId 0
      privKey = mkPrivKey poolId
      cs = mkPVCommittee privKey
  return $ PVSetup cs privKey

benchmarks :: [Benchmark]
benchmarks =
  [ env setupPV $ \pvSetup ->
      let PVSetup cs privKey = pvSetup
       in bgroup
            "persistent-vote"
            [ bench "create" $
                perRunEnv
                  ((\(_, eId, ebHash) -> BenchEnv (eId, ebHash)) <$> randomBenchInputs)
                  ( \(BenchEnv (eId, ebHash)) ->
                      case createPersistentVote cs privKey eId ebHash of
                        Left err -> error err
                        Right newVote -> void $ evaluate (pvVoteSignature newVote)
                  )
            , bench "verify" $
                perRunEnv
                  ( do
                      (_, eId, ebHash) <- randomBenchInputs
                      case createPersistentVote cs privKey eId ebHash of
                        Left err -> error $ "verify setup: " ++ err
                        Right vote -> return $ BenchEnv (eId, ebHash, vote)
                  )
                  ( \(BenchEnv (eId, ebHash, vote)) ->
                      void $ evaluate $ verifyLeiosVote cs eId ebHash (LeiosPersistentVote vote)
                  )
            ]
  ]
