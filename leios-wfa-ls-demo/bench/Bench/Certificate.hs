module Bench.Certificate (benchmarks) where

import Bench.Utils (
  BenchEnv (..),
  generateLinearStakeDist,
  generateParetoStakeDist,
  mkPoolId,
  mkPrivKey,
  randomBenchInputs,
  testNetworkId,
 )
import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Binary (ToCBOR (toCBOR))
import Cardano.Crypto.DSIGN.BLS12381 (SigDSIGN (SigBLS12381))
import Cardano.Leios.Certificate (Certificate (..), createCertificate, verifyCertificate)
import Cardano.Leios.Committee (OrderedSetOfParties (..), Party (..), mkOrderedSetOfParties)
import Cardano.Leios.Crypto (PrivateKeyLeios (..), SignatureLeios (..))
import Cardano.Leios.Types (ElectionId, EndorserBlockHash, PoolId)
import Cardano.Leios.Utils (createParties, toSkForBLS)
import Cardano.Leios.Vote (
  LeiosVote (..),
  createNonPersistentVote,
  createPersistentVote,
 )
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  wFA,
 )
import Cardano.Query (mkLocalNodeConnInfo, queryPoolDistrMap, renderQueryError)
import Codec.CBOR.Write (toStrictByteString)
import Control.Exception (evaluate)
import Criterion.Main (Benchmark, bench, bgroup, env, whnfIO)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv)

-- | Build PV and NPV votes for a bench run (not timed).
-- Takes up to pvCount PV votes (always succeed for PV parties) and up to
-- npvCount NPV votes by trying all npvPrivKeys under sortition.
buildVotes ::
  CommitteeSelection ->
  [PrivateKeyLeios] ->
  [PrivateKeyLeios] ->
  ElectionId ->
  EndorserBlockHash ->
  Int ->
  Int ->
  [LeiosVote]
buildVotes committee pvPrivKeys npvPrivKeys eId ebHash pvCount npvCount =
  pvVotes ++ take npvCount npvCandidates
  where
    nonce = praosNonce committee
    pvVotes =
      [ LeiosPersistentVote v
      | k <- take pvCount pvPrivKeys
      , Right v <- [createPersistentVote committee k eId ebHash]
      ]
    npvCandidates =
      [ LeiosNonPersistentVote v
      | k <- npvPrivKeys
      , Right v <- [createNonPersistentVote nonce committee k eId ebHash]
      ]

-- | A single named range benchmark group with create + verify benches.
-- Setup (vote creation, sortition) runs once; only createCertificate / verifyCertificate is timed.
rangeBench ::
  CommitteeSelection ->
  [PrivateKeyLeios] ->
  [PrivateKeyLeios] ->
  String ->
  Int ->
  Int ->
  Benchmark
rangeBench committee pvPrivKeys npvPrivKeys label pvCount npvCount =
  env
    ( do
        (_, eId, ebHash) <- randomBenchInputs
        -- Build votes once (expensive for NPV: tries all keys under sortition).
        -- createCertificate also forces all lazy vote-signature thunks so they are
        -- memoised for every subsequent benchmark iteration.
        let votes = buildVotes committee pvPrivKeys npvPrivKeys eId ebHash pvCount npvCount
        cert <- case createCertificate eId ebHash committee votes of
          Left err -> error $ "rangeBench setup: " ++ err
          Right c -> return c
        return $! BenchEnv (eId, ebHash, votes, cert)
    )
    ( \e ->
        let BenchEnv (eId, ebHash, votes, cert) = e
         in bgroup
              label
              [ bench "create" $
                  whnfIO $
                    case createCertificate eId ebHash committee votes of
                      Left err -> error $ "cert create bench: " ++ err
                      Right cert' -> case aggrVote cert' of
                        SignatureLeios (SigBLS12381 pt) -> evaluate pt
              , bench "verify" $
                  whnfIO $
                    evaluate $
                      verifyCertificate eId ebHash committee cert
              ]
    )

-- | Map a fraction to a count, returning 0 verbatim for 0 fractions
-- and clamping to at least 1 otherwise.
countFromFrac :: Double -> Int -> Int
countFromFrac frac n
  | frac == 0.0 = 0
  | n == 0 = 0
  | otherwise = max 1 (round (frac * fromIntegral n))

rangeLabel :: Int -> Int -> String
rangeLabel pvCount npvCount = "pv=" ++ show pvCount ++ ",npv=" ++ show npvCount

-- | 6-step pv-sweep: vary PV [1, 10%, 25%, 50%, 75%, 100%], NPV fixed at 0.
pvSweepRanges :: Int -> [(Int, Int)]
pvSweepRanges numPV =
  [ (max 1 (round (frac * fromIntegral numPV :: Double)), 0)
  | frac <- [1.0 / fromIntegral (max 1 numPV), 0.1, 0.25, 0.5, 0.75, 1.0]
  ]

-- | 6-step npv-sweep: vary NPV winners [0, 10%, 25%, 50%, 75%, 100%], PV fixed at 1.
npvSweepRanges :: Int -> [(Int, Int)]
npvSweepRanges numNPVWinners =
  [ (1, countFromFrac frac numNPVWinners)
  | frac <- [0.0, 0.1, 0.25, 0.5, 0.75, 1.0]
  ]

-- | 6-step diagonal: both vary (pvFrac, npvFrac) pairs.
diagonalRanges :: Int -> Int -> [(Int, Int)]
diagonalRanges numPV numNPVWinners =
  [ ( max 1 (round (pvFrac * fromIntegral numPV :: Double))
    , countFromFrac npvFrac numNPVWinners
    )
  | (pvFrac, npvFrac) <-
      [ (1.0 / fromIntegral (max 1 numPV), 1.0)
      , (0.1, 0.9)
      , (0.25, 0.75)
      , (0.5, 0.5)
      , (0.75, 0.25)
      , (1.0, 0.0)
      ]
  ]

-- | Build a worst-case bench group for a single synthetic distribution.
-- Each size n uses a committee where all n parties get persistent seats,
-- so all n votes go into the certificate.
-- 'genDist' is called once per size n during setup (not timed).
mkSyntheticGroup ::
  String ->
  (Int -> IO (Map.Map PoolId Rational)) ->
  [Int] ->
  IO Benchmark
mkSyntheticGroup distLabel genDist sizes = do
  sizebenches <- mapM mkOne sizes
  return $ bgroup distLabel sizebenches
  where
    mkOne n = do
      distr <- genDist n
      (nonce, _, _) <- randomBenchInputs
      let ps = createParties testNetworkId (Map.toList distr)
          -- committeeSize = n forces all n parties into persistent seats
          pvPrivKeys = map (mkPrivKey . mkPoolId) [0 .. n - 1]
      orderedPs <- case mkOrderedSetOfParties (fromIntegral n) ps of
        Left err -> error $ "mkSyntheticGroup " ++ distLabel ++ " n=" ++ show n ++ ": " ++ show err
        Right ops -> return ops
      let committee = wFA testNetworkId nonce orderedPs
          numPV = Map.size (persistentSeats committee)
      putStrLn $
        "cert synthetic/"
          ++ distLabel
          ++ "/n="
          ++ show n
          ++ ": numPV="
          ++ show numPV
      return $ rangeBench committee pvPrivKeys [] ("n=" ++ show n) n 0

-- | Synthetic worst-case benchmarks: Pareto and linear distributions,
-- across a range of committee sizes. Always runs (no node socket required).
syntheticBenchmarks :: IO Benchmark
syntheticBenchmarks = do
  let sizes = [100, 250, 500, 1000, 1500, 2000, 2500, 3000]
      alpha = 1.5 :: Double
  paretoBench <-
    mkSyntheticGroup
      ("pareto/alpha=" ++ show alpha)
      (generateParetoStakeDist alpha)
      sizes
  linearBench <-
    mkSyntheticGroup
      "linear"
      (\n -> return (generateLinearStakeDist n))
      sizes
  return $ bgroup "synthetic" [paretoBench, linearBench]

-- | Scaling benchmarks for graph generation: Pareto(α=0.5) and linear,
-- n ∈ [1000..3000]. Run with --match pattern "cert/synthetic/scaling" and
-- --json to feed into bench/plot_scaling.py.
scalingBenchmarks :: IO Benchmark
scalingBenchmarks = do
  let sizes = [1000, 1500, 2000, 2500, 3000]
      alpha = 0.5 :: Double
  paretoBench <-
    mkSyntheticGroup
      ("pareto/alpha=" ++ show alpha)
      (generateParetoStakeDist alpha)
      sizes
  linearBench <-
    mkSyntheticGroup
      "linear"
      (\n -> return (generateLinearStakeDist n))
      sizes
  return $ bgroup "scaling" [paretoBench, linearBench]

-- | Mainnet benchmarks: requires LEIOS_BENCH_NODE_SOCKET to be set.
-- Returns an empty list when the socket is unavailable.
mainnetBenchmarks :: IO [Benchmark]
mainnetBenchmarks = do
  mSocket <- lookupEnv "LEIOS_BENCH_NODE_SOCKET"
  case mSocket of
    Nothing -> do
      putStrLn "cert: LEIOS_BENCH_NODE_SOCKET not set; skipping mainnet certificate benchmarks"
      return []
    Just socketPath -> do
      mMagicStr <- lookupEnv "LEIOS_BENCH_NETWORK_MAGIC"
      let magic = maybe 764824073 read mMagicStr :: Int
          nId = Testnet (NetworkMagic (fromIntegral magic))
      poolDistrResult <- queryPoolDistrMap (mkLocalNodeConnInfo magic socketPath 0)
      case poolDistrResult of
        Left err -> do
          putStrLn $ "cert: query failed: " ++ renderQueryError err
          return []
        Right poolDistr -> do
          let ps = createParties nId (Map.toList poolDistr)
          case mkOrderedSetOfParties 575 ps of
            Left mkErr -> do
              putStrLn $ "cert: mkOrderedSetOfParties failed: " ++ show mkErr
              return []
            Right orderedPs -> do
              (nonce, _, _) <- randomBenchInputs
              let committee = wFA nId nonce orderedPs
                  numPV = Map.size (persistentSeats committee)
                  numNPVVoters = Map.size (voters (nonPersistentVoters committee))
              putStrLn $
                "cert: committee numPV="
                  ++ show numPV
                  ++ " numNPVVoters="
                  ++ show numNPVVoters
                  ++ " targetSize=575"

              -- Derive private keys for all parties (ordered by descending stake)
              let orderedPsList = parties orderedPs
                  mkKey p = PrivateKeyLeios (nId, toSkForBLS (poolId p))
                  pvPrivKeys = map mkKey (take numPV orderedPsList)
                  npvPrivKeys = map mkKey (drop numPV orderedPsList)

              -- Sample one (eId, ebHash) to count NPV winners and log cert sizes
              (_, sampleEId, sampleEbHash) <- randomBenchInputs
              let sampleNonce = praosNonce committee
                  numNPVWinners =
                    length
                      [ ()
                      | k <- npvPrivKeys
                      , Right _ <- [createNonPersistentVote sampleNonce committee k sampleEId sampleEbHash]
                      ]
              putStrLn $
                "cert: npvWinners="
                  ++ show numNPVWinners
                  ++ "/"
                  ++ show numNPVVoters
                  ++ " (sample draw)"

              -- Helper: build and return range benches, logging cert size for each step
              let buildGroup groupName ranges = do
                    rangebenches <- mapM (logAndBench groupName) ranges
                    return $ bgroup groupName rangebenches
                  logAndBench groupName (pvc, npvc) = do
                    let lbl = rangeLabel pvc npvc
                        votes = buildVotes committee pvPrivKeys npvPrivKeys sampleEId sampleEbHash pvc npvc
                        certSizeStr = case createCertificate sampleEId sampleEbHash committee votes of
                          Left e -> "error: " ++ e
                          Right cert -> show (BS.length (toStrictByteString (toCBOR cert))) ++ " bytes"
                    putStrLn $ "cert mainnet/" ++ groupName ++ "/" ++ lbl ++ ": size=" ++ certSizeStr
                    return $ rangeBench committee pvPrivKeys npvPrivKeys lbl pvc npvc

              pvBench <- buildGroup "pv-sweep" (pvSweepRanges numPV)
              npvBench <- buildGroup "npv-sweep" (npvSweepRanges numNPVWinners)
              diagBench <- buildGroup "diagonal" (diagonalRanges numPV numNPVWinners)
              return [bgroup "mainnet" [pvBench, npvBench, diagBench]]

benchmarks :: IO [Benchmark]
benchmarks = do
  mainnet <- mainnetBenchmarks
  synthetic <- syntheticBenchmarks
  scaling <- scalingBenchmarks
  return (mainnet ++ [synthetic, scaling])
