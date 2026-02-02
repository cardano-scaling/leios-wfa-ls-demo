{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Query
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
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
        Right m -> print $ take 3 (Map.toList m)
