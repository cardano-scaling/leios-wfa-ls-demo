{-# LANGUAGE DataKinds #-}

module Cardano.Leios.Types where

import Cardano.Api.Ledger (KeyHash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Keys (KeyRole (..))
import Data.Word (Word64)

-- | A type that represent the pool ID. This is the blake2b-224 hash of
-- the ed25519 cold key of the stakepool.
type PoolID = KeyHash StakePool

-- | Identifier for the voting round (derived from the slot number of the RB that announced the target EB)
-- TODO: The Shelley CDDL sets this at 8 bytes, but maybe we should link this to the ledger slot type
-- Also note that the VRF should use the epoch Praos nonce with its input for entropy.
type ElectionID = Word64

-- | Phantom type that represent and EB.
data EndorserBlock

-- | The hash of the EB. Note that `HASH` is a synonym for blake2b_256.
type EndorserBlockHash = Hash.Hash HASH EndorserBlock

-- | A value between zero and one, representing the relative stake
-- A `Party` has in Praos.
type RelativeStake = Rational

-- | The weight we assign each seat in the `CommitteeSelection`
type Weight = Rational
