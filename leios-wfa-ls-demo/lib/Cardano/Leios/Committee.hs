{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.Committee (
  CommitteeSize,
  Party (..),
  PoolId,
  OrderedSetOfParties (..),
  mkOrderedSetOfParties,
) where

import Cardano.Leios.Crypto
import Cardano.Leios.Types
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Word (Word16)

-- | A type wrapper around `Word16` representing the maximal committee
-- size the protocol targets. Note that this the protocol on avarage will
-- have a committee of this size, since the non-persistent seats
-- are stochasticly assigned based on a VRF.
type CommitteeSize = Word16

-- | A type representing a party in the Leios protocol.
-- This type assumes that the public key is valid,
-- e.g, the Proof of Possesion of this key has been checked.
data Party = Party
  { poolId :: PoolId
  -- ^ The `poolId` is not strictly needed in this type
  --   keeping it for know as the bls key will be derived from it
  --   in tests
  , publicVoteKey :: PublicKeyLeios 'Vote
  , stake :: Rational
  }
  deriving (Show)

-- | A type representing the ordered (from large to small) set of
-- stakepools. To construct this use `mkOrderedSetOfParties` to ensure
-- the underlying list is ordered. This type also contains the committee
-- size.
data OrderedSetOfParties = OrderedSetOfParties
  { parties :: [Party]
  , committeeSize :: CommitteeSize
  }

-- | A type representing the possible ways we can fail when
-- constructing a ordered set of paries. These are
--
-- 1. We want a committee size that is larger than #pools
-- 2. We are limiting the number of pools to `(maxBound :: Word16)
--    exeeding this fill also error.
--
-- Note that we do not check that stake is never negative or the
-- sum is not 1, this is assumed.
data MkOrderedSetOfPartiesError
  = CommitteeTooLarge
      { requestedCommitteeSize :: CommitteeSize
      , numberOfPools :: Int
      }
  | TooManyPools
      { numberOfPools :: Int
      , maxPoolsAllowed :: Word16
      }
  deriving (Show)

-- | A function to safely construct a `OrderedSetOfParties` from a
-- list of stakepools identified by their `PoolId`,
-- their stake `Rational` and the `committeeSize`.
--
-- Note that we require the committee size to be smaller or equal
-- to the number of pools. We further require that the number of
-- pools is never more than the `maxBound` of an `Word16`. And lastly
-- that the total sum of the stake is one. Perhaps we could also check
-- for negative stake, but I think we can assume that the node returns that.
mkOrderedSetOfParties ::
  CommitteeSize -> [Party] -> Either MkOrderedSetOfPartiesError OrderedSetOfParties
mkOrderedSetOfParties comSize poolDistr
  | numPools > fromIntegral (maxBound :: Word16) =
      Left $
        TooManyPools
          { numberOfPools = numPools
          , maxPoolsAllowed = maxBound
          }
  | fromIntegral @Word16 @Int comSize > numPools =
      Left $
        CommitteeTooLarge
          { requestedCommitteeSize = comSize
          , numberOfPools = numPools
          }
  | otherwise =
      Right $
        OrderedSetOfParties
          { parties = sortOn (Down . stake) poolDistr
          , committeeSize = comSize
          }
  where
    numPools = length poolDistr
