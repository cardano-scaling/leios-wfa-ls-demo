{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Query (
    mkLocalNodeConnInfo,
    queryPoolDistrMap,
    QueryError (..),
    renderQueryError,
) where

import Cardano.Api hiding (Hash)
import Cardano.Api.Internal.Query (PoolDistribution (..), decodePoolDistribution)
import Cardano.Api.Ledger (KeyHash, StandardCrypto)
import qualified Cardano.Api.Network as Network
import Cardano.Crypto.VRF (VerKeyVRF)
import Cardano.Ledger.Hashes (HASH, Hash)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Protocol.Crypto (VRF)
import Control.Error (fmapLT, note, (??))
import Control.Exception (SomeException, try)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Ouroboros.Consensus.Shelley.Ledger.Query.Types as OC

mkLocalNodeConnInfo :: Int -> FilePath -> Int -> LocalNodeConnectInfo
mkLocalNodeConnInfo testnetMagic nodeSocket epochSlots =
    LocalNodeConnectInfo
        { localConsensusModeParams = CardanoModeParams (EpochSlots (fromIntegral epochSlots))
        , localNodeNetworkId = Testnet (NetworkMagic (fromIntegral testnetMagic))
        , localNodeSocketPath = fromString nodeSocket
        }

data QueryError
    = CurrentEra !String
    | NotBabbageOrLater !AnyCardanoEra
    | AcquireOrProtocol !String
    | EraMismatch !String
    | DecodePoolDistr !String
    | Unexpected !String
    deriving (Show)

renderQueryError :: QueryError -> String
renderQueryError = \case
    CurrentEra e ->
        "QueryCurrentEra failed: " <> e
    NotBabbageOrLater era ->
        "PoolDistribution query is only supported in Babbage era or later. Current era: " <> show era
    AcquireOrProtocol e ->
        "PoolDistribution query failed: " <> e
    EraMismatch e ->
        "PoolDistribution era mismatch: " <> e
    DecodePoolDistr e ->
        "decodePoolDistribution failed: " <> e
    Unexpected e ->
        "Unexpected error: " <> e

unpackPoolDistr :: OC.PoolDistr crypto -> Map.Map (KeyHash StakePool) Rational
unpackPoolDistr (OC.PoolDistr m) = Map.map OC.individualPoolStake m

queryPoolDistrMap ::
    LocalNodeConnectInfo ->
    IO (Either QueryError (Map.Map (KeyHash StakePool) Rational))
queryPoolDistrMap conn = runExceptT $ do
    let tip = Network.VolatileTip

    AnyCardanoEra era <-
        fmapLT (CurrentEra . show) $
            ExceptT $
                runExceptT $
                    queryNodeLocalState conn tip QueryCurrentEra

    babbageEraOnwards <-
        hoistEither $
            note (NotBabbageOrLater (AnyCardanoEra era)) $
                forEraMaybeEon @BabbageEraOnwards era

    serialisedPoolDistrEither <-
        ExceptT $ do
            r <-
                executeLocalStateQueryExpr conn tip $
                    queryPoolDistribution babbageEraOnwards Nothing
            pure $ case r of
                Left uerr -> Left (AcquireOrProtocol (show uerr))
                Right (Left mm) -> Left (EraMismatch (show mm))
                Right (Right serialised) -> Right serialised

    serialisedPoolDistr <- hoistEither $
        case serialisedPoolDistrEither of
            Left err -> Left (EraMismatch (show err))
            Right ser -> Right ser

    let shelleyBasedEra = convert babbageEraOnwards
    PoolDistribution poolDistr <-
        hoistEither $
            first (DecodePoolDistr . show) $
                decodePoolDistribution shelleyBasedEra serialisedPoolDistr

    pure (unpackPoolDistr poolDistr)
