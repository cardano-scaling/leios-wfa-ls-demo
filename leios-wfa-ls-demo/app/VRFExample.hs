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

              -- Message to sign with VRF (e.g., EB hash + slot number)
              message = BSC.pack "example-endorser-block-hash:slot-12345"

              -- For each non-persistent voter, sign the message and check sortition
              results = map (checkVoter nId message n2) nonPersistentCandidates
              totalSeats = sum results
              numVoters = length nonPersistentCandidates
              votersWithSeats = length $ filter (> 0) results

          putStrLn "=== VRF Local Sortition Example ==="
          putStrLn $ "Target committee size:    " <> show targetCommitteeSize
          putStrLn $ "Persistent seats:         " <> show numPersistentSeats
          putStrLn $ "Target n2 (remaining):    " <> show n2
          putStrLn $ "Number of candidates:     " <> show numVoters
          putStrLn $
            "Voters winning seats:     "
              <> show votersWithSeats
              <> " ("
              <> printf "%.1f%%" (100 * fromIntegral votersWithSeats / fromIntegral numVoters :: Double)
              <> ")"
          putStrLn $ "Total seats won:          " <> show totalSeats
          putStrLn $ "Expected seats (E[X]):    " <> show n2
          putStrLn $
            "Ratio (actual/expected):  " <> printf "%.3f" (fromIntegral totalSeats / fromIntegral n2 :: Double)
          putStrLn ""
          putStrLn "Seat distribution:"
          let seatCounts = [(seats, length $ filter (== seats) results) | seats <- [0 .. maximum results]]
          mapM_
            ( \(seats, count) ->
                putStrLn $ "  " <> show seats <> " seats: " <> show count <> " voters"
            )
            seatCounts

-- Check one voter: sign message with VRF key, run sortition
checkVoter :: NetworkId -> BSC.ByteString -> Int -> (PoolId, NonPersistentVoter) -> Int
checkVoter nId message n2 (pId, voter) = seats
  where
    vrfSk = toSkForVRF pId nId

    -- Sign the message with VRF key to get VRF output
    vrfOutput = signWithRoleLeios message vrfSk

    -- Run local sortition to determine number of seats
    seats = checkLeaderValueLeios vrfOutput (stakeNonPersistentVoter voter) (fromIntegral n2)
