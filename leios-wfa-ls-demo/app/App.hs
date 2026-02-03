{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Seed
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Query
import Cardano.WeightedFaitAccompli
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

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
                <> help "network magic (e.g. 2 for preview)"
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

    let localNodeConnInfo = mkLocalNodeConnInfo (optTestnetMagic o) (optNodeSocket o) 0
    eMap <- queryPoolDistrMap localNodeConnInfo
    case eMap of
        Left err -> hPutStrLn stderr (renderQueryError err) >> exitFailure
        Right m -> do
            let pools = mkOrderedSetOfParties $ Map.toList m
                committeeSize = 1000 :: CommitteeSize
            print $ last $ appointSeats committeeSize pools

    let seedLen = fromIntegral (seedSizeDSIGN (Proxy @BLS12381MinVerKeyDSIGN))
        seedBytes = BS.replicate seedLen 42
        seed = mkSeedFromBytes seedBytes
        skVer = genKeyDSIGN @BLS12381MinVerKeyDSIGN seed
        vkVer = deriveVerKeyDSIGN @BLS12381MinVerKeyDSIGN skVer
        simpleMsg = "Hello World" :: BS.ByteString
        emptyBLSContext = BLS12381SignContext Nothing Nothing
        sigVer = signDSIGN @BLS12381MinVerKeyDSIGN emptyBLSContext simpleMsg skVer
    print $ verifyDSIGN @BLS12381MinVerKeyDSIGN emptyBLSContext vkVer simpleMsg sigVer
