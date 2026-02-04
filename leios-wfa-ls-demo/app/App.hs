{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Leios.Utils
import Cardano.Leios.WeightedFaitAccompli
import Cardano.Query
import qualified Data.Map.Strict as Map
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
            -- Note that on preview we have ~611 pools
            let committeeSize = 600 :: CommitteeSize
                parties = createParties $ Map.toList m
            print $ head parties
            case mkOrderedSetOfParties committeeSize parties of
                Left err ->
                    hPutStrLn stderr $ "'mkOrderedSetOfParties' error: " <> show err
                Right pools -> do
                    print $ wFALS pools
