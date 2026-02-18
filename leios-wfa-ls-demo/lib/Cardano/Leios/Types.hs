{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.Types where

import Cardano.Api.Ledger (KeyHash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Keys (KeyRole (..))
import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | A type that represents the pool ID. This is the blake2b-224 hash of
-- the ed25519 cold key of the stake pool.
type PoolId = KeyHash StakePool

-- | Identifier for the voting round (derived from the slot number of the RB that announced the target EB)
type ElectionId = Word64

-- | Phantom type that represents a serialized EB.
newtype EndorserBlock = EndorserBlock ByteString

-- | The hash of the EB. Note that `HASH` is a synonym for blake2b_256.
type EndorserBlockHash = Hash.Hash HASH EndorserBlock

-- | Helper function to hash an `EndorserBlock` into an `EndorserBlockHash`
endorserBlockHash :: EndorserBlock -> EndorserBlockHash
endorserBlockHash = Hash.hashWith @HASH (\(EndorserBlock b) -> b)

-- | A value between zero and one, representing the relative stake
-- a `Party` has in Praos.
type RelativeStake = Rational

-- | The weight we assign to each seat in the `CommitteeSelection`
type Weight = Rational
