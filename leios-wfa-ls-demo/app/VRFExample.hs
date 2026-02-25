{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Api.Shelley (makePraosNonce)
import Cardano.Leios.Committee
import Cardano.Leios.Crypto
import Cardano.Leios.LocalSortition
import Cardano.Leios.Utils
import Cardano.Leios.WeightedFaitAccompli
import Cardano.Query
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- Derive VRF secret key from PoolId (similar to Utils.hs for BLS keys)
toSkForVRF :: PoolId -> NetworkId -> PrivateKeyLeios 'VRF
toSkForVRF pId nId = PrivateKeyLeios (nId, toSkForBLS pId)

data Opts = Opts
  { optTestnetMagic :: !Int
  , optNodeSocket :: !FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
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

main :: IO ()
main = do
  o <-
    execParser $
      info
        (optsParser <**> helper)
        (fullDesc <> progDesc "VRF local sortition example")

  let networkMagic = optTestnetMagic o
      localNodeConnInfo = mkLocalNodeConnInfo networkMagic (optNodeSocket o) 0
  poolDistrResult <- queryPoolDistrMap localNodeConnInfo
  case poolDistrResult of
    Left err -> hPutStrLn stderr (renderQueryError err) >> exitFailure
    Right m -> do
      let targetCommitteeSize = 575 :: CommitteeSize
          nId =
            if networkMagic == 764824073
              then Mainnet
              else Testnet (NetworkMagic (fromIntegral @Int @Word32 networkMagic))
          unOrderedParties = createParties nId $ Map.toList m
      case mkOrderedSetOfParties targetCommitteeSize unOrderedParties of
        Left err ->
          hPutStrLn stderr $ "'mkOrderedSetOfParties' error: " <> show err
        Right pools -> do
          let epochNonce = makePraosNonce $ BSC.pack "some-random-nonce"
              committee = wFA epochNonce pools

              -- n2 is the difference between target committee size and persistent voters
              numPersistentSeats = Map.size . persistentSeats $ committee
              n2 = fromIntegral @CommitteeSize @Int targetCommitteeSize - numPersistentSeats

              nonPersistentCandidates = Map.toList . voters . nonPersistentVoters $ committee
              numVoters = length nonPersistentCandidates

              -- Calculate total remaining stake (for renormalization)
              totalRemainingStake = sum [stakeNonPersistentVoter v | (_, v) <- nonPersistentCandidates]

              -- Renormalize stakes relative to remaining non-persistent stake
              renormalizedCandidates = [(pId, v, stakeNonPersistentVoter v / totalRemainingStake) | (pId, v) <- nonPersistentCandidates]

              -- Number of slots to simulate
              numSlots = 300 :: Int

              -- Run sortition for each slot
              slotResults = [runSlotSortition nId slot n2 renormalizedCandidates | slot <- [1 .. numSlots]]

              -- Statistics across all slots
              totalSeatsPerSlot = map sum slotResults
              avgSeatsPerSlot = fromIntegral (sum totalSeatsPerSlot) / fromIntegral numSlots :: Double
              minSeats = minimum totalSeatsPerSlot
              maxSeats = maximum totalSeatsPerSlot

              -- Calculate standard deviation
              variance =
                sum [(fromIntegral seats - avgSeatsPerSlot) ^ (2 :: Int) | seats <- totalSeatsPerSlot]
                  / fromIntegral numSlots
              stdDev = sqrt variance :: Double

              -- Count voters winning seats across all slots
              totalWinners = sum [length $ filter (> 0) results | results <- slotResults]
              avgWinnersPerSlot = fromIntegral totalWinners / fromIntegral numSlots :: Double

              -- Distribution of seats won per voter (total occurrences across all slots)
              maxSeatsWon = maximum $ concat slotResults
              totalCandidateSlots = numVoters * numSlots
              seatDistribution =
                [ (k, sum [length $ filter (== k) results | results <- slotResults])
                | k <- [0 .. maxSeatsWon]
                ]

          putStrLn "=== VRF Local Sortition Example ==="
          putStrLn $ "Target committee size:    " <> show targetCommitteeSize
          putStrLn $ "Persistent seats:         " <> show numPersistentSeats
          putStrLn $ "Target n2 (remaining):    " <> show n2
          putStrLn $ "Number of candidates:     " <> show numVoters
          putStrLn $ "Number of slots:          " <> show numSlots
          putStrLn ""
          putStrLn $ "=== Results across " <> show numSlots <> " slots ==="
          putStrLn $ "Average seats per slot:   " <> printf "%.2f" avgSeatsPerSlot
          putStrLn $ "Expected seats (E[X]):    " <> show n2
          putStrLn $ "Ratio (actual/expected):  " <> printf "%.3f" (avgSeatsPerSlot / fromIntegral n2)
          putStrLn $ "Standard deviation:       " <> printf "%.2f" stdDev
          putStrLn $ "Min seats in a slot:      " <> show minSeats
          putStrLn $ "Max seats in a slot:      " <> show maxSeats
          putStrLn $
            "Avg winners per slot:     "
              <> printf "%.2f" avgWinnersPerSlot
              <> " ("
              <> printf "%.1f%%" (100 * avgWinnersPerSlot / fromIntegral numVoters)
              <> ")"
          putStrLn ""
          putStrLn $ "=== Seat distribution per voter (across " <> show numSlots <> " slots) ==="
          mapM_
            ( \(seats, count) -> do
                let percentage = 100 * fromIntegral count / fromIntegral totalCandidateSlots :: Double
                putStrLn $
                  "  "
                    <> show seats
                    <> " seat(s): "
                    <> show count
                    <> " occurrences ("
                    <> printf "%.2f%%" percentage
                    <> ")"
            )
            seatDistribution

-- Run sortition for a single slot
runSlotSortition :: NetworkId -> Int -> Int -> [(PoolId, NonPersistentVoter, Rational)] -> [Int]
runSlotSortition nId slotNum n2 candidates = map (checkVoter nId message n2) candidates
  where
    -- Message includes slot number to ensure different VRF outputs per slot
    message = BSC.pack $ "example-endorser-block-hash:slot-" <> show slotNum

-- Check one voter: sign message with VRF key, run sortition
checkVoter :: NetworkId -> BSC.ByteString -> Int -> (PoolId, NonPersistentVoter, Rational) -> Int
checkVoter nId message n2 (pId, _voter, normalizedStake) = seats
  where
    vrfSk = toSkForVRF pId nId

    -- Sign the message with VRF key to get VRF output
    vrfOutput = signWithRoleLeios message vrfSk

    -- Run local sortition with renormalized stake
    seats = checkLeaderValueLeios vrfOutput normalizedStake (fromIntegral n2)
