{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.Utils where

import Cardano.Api (NetworkId)
import Cardano.Api.Ledger (hashToBytes)
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Hashes (KeyHash (..))
import Cardano.Leios.Committee (Party (..), PoolId)
import Cardano.Leios.Crypto (KeyRoleLeios (..), PublicKeyLeios (..))
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))

-- Note that the following is insecure crypto usage and should not be used anywhere.

toFixedLen :: Int -> BS.ByteString -> BS.ByteString
toFixedLen n bs = BS.take n (bs <> BS.replicate n 0)

-- For ease of use we convert the 28 byte long hash of the stake pool
-- to a BLS signing key seed by appending zeros to make it length 32.
-- This is nice for testing, as we can then generate signatures
-- for a pool easily while the ledger does not know about BLS keys yet.
toSeedForBLS :: PoolId -> Seed
toSeedForBLS = mkSeedFromBytes . toFixedLen seedLen . hashToBytes . unKeyHash
  where
    seedLen = fromIntegral (seedSizeDSIGN (Proxy @BLS12381MinSigDSIGN))

toSkForBLS :: PoolId -> SignKeyDSIGN BLS12381MinSigDSIGN
toSkForBLS = genKeyDSIGN @BLS12381MinSigDSIGN . toSeedForBLS

toVerKeyForBLS :: PoolId -> NetworkId -> PublicKeyLeios 'Vote
toVerKeyForBLS pId nId = PublicKeyLeios (nId, (deriveVerKeyDSIGN @BLS12381MinSigDSIGN . toSkForBLS) pId)

createParties :: NetworkId -> [(PoolId, Rational)] -> [Party]
createParties nId = map (\(poolIdHash, stk) -> Party poolIdHash (toVerKeyForBLS poolIdHash nId) stk)
