{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Api.Shelley (makePraosNonce)
import Cardano.Leios.Committee
import Cardano.Leios.Utils
import Cardano.Leios.WeightedFaitAccompli
import Cardano.Query
import Cardano.Utils
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

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
        (fullDesc <> progDesc "Query a Cardano node for data")

  let networkMagic = optTestnetMagic o
      localNodeConnInfo = mkLocalNodeConnInfo networkMagic (optNodeSocket o) 0
  poolDistrResult <- queryPoolDistrMap localNodeConnInfo
  case poolDistrResult of
    Left err -> hPutStrLn stderr (renderQueryError err) >> exitFailure
    Right m -> do
      -- Note that on preview we have ~611 pools
      -- on mainnet this is ~3000 where ~800 have ~80% of stake
      let targetCommitteeSize = 500 :: CommitteeSize
          nId =
            if networkMagic == 764824073
              then Mainnet
              else Testnet (NetworkMagic (fromIntegral @Int @Word32 networkMagic)) :: NetworkId
          unOrderedParties = createParties nId $ Map.toList m
      case mkOrderedSetOfParties targetCommitteeSize unOrderedParties of
        Left err ->
          hPutStrLn stderr $ "'mkOrderedSetOfParties' error: " <> show err
        Right pools -> do
          let epochNonce = makePraosNonce $ BSC.pack "some-random-nonce"
              committee = wFA epochNonce pools
              numPersistent =
                (Map.size . persistentSeats) committee
              sumStakePersistentSeats :: Double
              sumStakePersistentSeats = (fromRational . sum . fmap weightPersistentSeat . persistentSeats) committee
              numNonPersistent =
                (Map.size . voters . nonPersistentVoters) committee
              stakePerNonPersistentSeat :: Double
              stakePerNonPersistentSeat = (fromRational . weightPerNonPersistentSeat . nonPersistentVoters) committee

          writeStakeCSV "stake.csv" numPersistent (fromIntegral @CommitteeSize @Int targetCommitteeSize) pools
          putStrLn $ "Target committee size  " <> show targetCommitteeSize
          putStrLn $
            "Persistent voters:     "
              <> show numPersistent
              <> " (total stake: "
              <> printf "%.2f%%" (100 * sumStakePersistentSeats)
              <> ")"
          putStrLn $
            "Non-persistent candidates: "
              <> show numNonPersistent
              <> " (stake per seat: "
              <> printf "%.2f%%" (100 * stakePerNonPersistentSeat)
              <> ")"
