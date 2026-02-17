{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.Utils where

import Cardano.Api.Ledger (hashToBytes)
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Hashes (KeyHash (..))
import Cardano.Leios.Committee (Party (..), PoolID)
import Cardano.Leios.Crypto (KeyRoleLeios (..), PublicKeyLeios (..))
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))

-- Note that the below is insecure crypto usage and should not be used anywhere.

toFixedLen :: Int -> BS.ByteString -> BS.ByteString
toFixedLen n bs = BS.take n (bs <> BS.replicate n 0)

-- For easy of use we convert the 28 byte long hash of the stakepool
-- to a bls signing key seed by appending zeros to make it length 32.
-- This is nice for testing, as we can on the generate signatures
-- for a pool easily while the ledger does not know about bls keys yet.
toSeedForBLS :: PoolID -> Seed
toSeedForBLS = mkSeedFromBytes . toFixedLen seedLen . hashToBytes . unKeyHash
  where
    seedLen = fromIntegral (seedSizeDSIGN (Proxy @BLS12381MinSigDSIGN))

toSkForBLS :: PoolID -> SignKeyDSIGN BLS12381MinSigDSIGN
toSkForBLS = genKeyDSIGN @BLS12381MinSigDSIGN . toSeedForBLS

toVerKeyForBLS :: PoolID -> PublicKeyLeios 'Vote
toVerKeyForBLS = PublicKeyLeios . deriveVerKeyDSIGN @BLS12381MinSigDSIGN . toSkForBLS

createParties :: [(PoolID, Rational)] -> [Party]
createParties = map (\(hsh, stk) -> Party hsh (toVerKeyForBLS hsh) stk)
