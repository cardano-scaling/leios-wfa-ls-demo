{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Api.Shelley (makePraosNonce)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (KeyHash (..))
import Cardano.Leios.Committee (
  CommitteeSize,
  OrderedSetOfParties (..),
  Party (..),
  PoolId,
  mkOrderedSetOfParties,
 )
import Cardano.Leios.Crypto (
  KeyRoleLeios (..),
  OutputVRF,
  PrivateKeyLeios (..),
  coercePrivateKeyLeios,
  signWithRoleLeios,
 )
import Cardano.Leios.LocalSortition (checkLeaderValueLeios)
import Cardano.Leios.Utils (createParties, toSkForBLS, toVerKeyForBLS)
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  stakeNonPersistentVoter,
  wFA,
 )
import Cardano.Query
import qualified Data.ByteString.Char8 as BSC
import Data.List (group, sort, transpose)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Word (Word32)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

data Opts = Opts
  { optMode :: !Mode
  , optRounds :: !Int
  }

data Mode
  = RealNetwork !Int !FilePath !Int
  | Generated !Int !Int

modeParser :: Parser Mode
modeParser =
  subparser
    ( command
        "real"
        ( info
            realNetworkParser
            (progDesc "Use real network data from a Cardano node")
        )
        <> command
          "generated"
          ( info
              generatedParser
              (progDesc "Use randomly generated committee data")
          )
    )

realNetworkParser :: Parser Mode
realNetworkParser =
  RealNetwork
    <$> option
      auto
      ( long "network-magic"
          <> metavar "INT"
          <> help "network magic (e.g. 764824073 for mainnet)"
      )
    <*> strOption
      ( long "socket-path"
          <> metavar "FILE"
          <> help "Path to node.socket"
      )
    <*> option
      auto
      ( long "committee-size"
          <> metavar "INT"
          <> value 575
          <> showDefault
          <> help "Target committee size"
      )

generatedParser :: Parser Mode
generatedParser =
  Generated
    <$> option
      auto
      ( long "num-pools"
          <> metavar "INT"
          <> value 100
          <> showDefault
          <> help "Number of pools to generate"
      )
    <*> option
      auto
      ( long "committee-size"
          <> metavar "INT"
          <> value 100
          <> showDefault
          <> help "Target committee size"
      )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> modeParser
    <*> option
      auto
      ( long "rounds"
          <> metavar "INT"
          <> value 10000
          <> showDefault
          <> help "Number of simulation rounds"
      )

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Simulate local sortition and compare expected vs realized seat allocation"
            <> header "simulate-local-sortition - Statistical verification of Leios local sortition"
        )

  case optMode opts of
    RealNetwork magic socket comSize -> do
      let localNodeConnInfo = mkLocalNodeConnInfo magic socket 0
      poolDistrResult <- queryPoolDistrMap localNodeConnInfo
      case poolDistrResult of
        Left err -> hPutStrLn stderr (renderQueryError err) >> exitFailure
        Right m -> do
          let nId =
                if magic == 764824073
                  then Mainnet
                  else Testnet (NetworkMagic (fromIntegral @Int @Word32 magic))
              unOrderedParties = createParties nId $ Map.toList m
          case mkOrderedSetOfParties (fromIntegral comSize) unOrderedParties of
            Left err -> hPutStrLn stderr $ "mkOrderedSetOfParties error: " <> show err
            Right osp -> runSimulation nId osp (optRounds opts)
    Generated nPools comSize -> do
      let osp = generateCommittee nPools comSize
          nId = Testnet (NetworkMagic 42)
      runSimulation nId osp (optRounds opts)

runSimulation :: NetworkId -> OrderedSetOfParties -> Int -> IO ()
runSimulation nId osp numRounds = do
  let epochNonce = makePraosNonce $ BSC.pack "simulation-nonce"
      committee = wFA nId epochNonce osp
      nonPersVoters = nonPersistentVoters committee
      persistentCount = Map.size (persistentSeats committee)
      n2 = fromIntegral @CommitteeSize @Integer (committeeSize osp) - fromIntegral persistentCount

  putStrLn $ "Committee size:         " <> show (committeeSize osp)
  putStrLn $ "Persistent voters:      " <> show persistentCount
  putStrLn $ "Non-persistent seats:   " <> show n2
  putStrLn ""

  if n2 <= 0
    then putStrLn "Error: no non-persistent seats available"
    else do
      putStrLn $ "Running " <> show numRounds <> " rounds..."
      putStrLn ""

      let poolResults = runMultipleRounds numRounds n2 nId nonPersVoters
          totalSeatsPerRound = map sum (transpose $ Map.elems poolResults)
          avgTotalSeats = average totalSeatsPerRound
          expectedPerRound = fromIntegral n2
          relativeError = abs (avgTotalSeats - expectedPerRound) / expectedPerRound

      putStrLn "=== TOTAL SEATS ==="
      printf "Expected per round: %.2f\n" expectedPerRound
      printf "Actual average:     %.2f\n" avgTotalSeats
      printf "Relative error:     %.2f%%\n" (relativeError * 100)
      putStrLn ""

      let seatDistribution = calculateSeatDistribution poolResults
          seatMap = Map.fromList seatDistribution
          distribution0to10 = [(k, Map.findWithDefault 0 k seatMap) | k <- [0 .. 10]]
          totalFromDist = sum [seats * fromIntegral count | (seats, count) <- distribution0to10]
          expectedCumulative = fromIntegral numRounds * n2

      putStrLn "=== SEAT DISTRIBUTION ==="
      mapM_ printSeatDistribution distribution0to10
      printf "Total: %d (expected: %d)\n" totalFromDist expectedCumulative
      putStrLn ""

generateVRFOutputForRound :: PoolId -> NetworkId -> Int -> OutputVRF
generateVRFOutputForRound pId nId roundNum =
  let vrfKey = coercePrivateKeyLeios @'Vote @'VRF (PrivateKeyLeios (nId, toSkForBLS pId))
      msg = BSC.pack $ "local-sortition-round-" ++ show roundNum
   in signWithRoleLeios msg vrfKey

runLocalSortitionRound ::
  Int ->
  Integer ->
  NetworkId ->
  NonPersistentLocalSortition ->
  Map.Map PoolId Int
runLocalSortitionRound roundNum n2 nId nonPersVoters =
  Map.mapWithKey checkVoter (voters nonPersVoters)
  where
    checkVoter pId voter =
      let vrfOutput = generateVRFOutputForRound pId nId roundNum
          σ = stakeNonPersistentVoter voter
       in case checkLeaderValueLeios vrfOutput σ n2 of
            Right seats -> seats
            Left err -> error $ "Round " ++ show roundNum ++ " pool " ++ show pId ++ ": " ++ show err

runMultipleRounds ::
  Int ->
  Integer ->
  NetworkId ->
  NonPersistentLocalSortition ->
  Map.Map PoolId [Int]
runMultipleRounds numRounds n2 nId nonPersVoters =
  let roundResults = [runLocalSortitionRound r n2 nId nonPersVoters | r <- [1 .. numRounds]]
      collectForPool pId = [Map.findWithDefault 0 pId result | result <- roundResults]
   in Map.fromList [(pId, collectForPool pId) | pId <- Map.keys (voters nonPersVoters)]

average :: [Int] -> Double
average [] = 0
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

calculateSeatDistribution :: Map.Map PoolId [Int] -> [(Int, Int)]
calculateSeatDistribution poolResults =
  let allSeats = concat $ Map.elems poolResults
      grouped = group $ sort allSeats
   in map (\g -> (head g, length g)) grouped

printSeatDistribution :: (Int, Int) -> IO ()
printSeatDistribution (seats, count) =
  printf "%2d seats: %7d occurrences\n" seats count

generateCommittee :: Int -> Int -> OrderedSetOfParties
generateCommittee numPools comSize =
  let rawStakes = [1 % fromIntegral i | i <- [1 .. numPools]]
      totalStake = sum rawStakes
      normalizedStakes = map (/ totalStake) rawStakes
      partyList = zipWith mkParty [0 ..] normalizedStakes
   in OrderedSetOfParties
        { parties = partyList
        , committeeSize = fromIntegral comSize
        }

mkParty :: Int -> Rational -> Party
mkParty idx stk =
  let pId = KeyHash $ Hash.castHash $ Hash.hashWith id $ BSC.pack (show idx)
      nId = Testnet (NetworkMagic 42)
   in Party
        { poolId = pId
        , publicVoteKey = toVerKeyForBLS pId nId
        , stake = stk
        }
