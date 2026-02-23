{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentVoters,
  NonPersistentVoter (..),
  PersistentSeats,
  PersistentSeat (..),
  NonPersistentLocalSortition (..),
  PersistentVoterIndex,
  findNonPersistentVoterByPublicKey,
  findPersistentSeatByPublicKey,
  findIStar,
  findIStarAcc,
  wFA,
  rho,
) where

import Cardano.Api (PraosNonce)
import Cardano.Leios.Committee
import Cardano.Leios.Crypto
import Data.List (find)
import qualified Data.Map as Map
import Data.Word (Word16)

{-
This module implements the core of Weighted Fait Accompli.

The code below closely follows the paper found here: https://dl.acm.org/doi/10.1145/3576915.3623194

The why:

The core idea of the above paper is that one can reduce randomness where it is not needed
to get a smaller committee (so that hopefully consensus is reached faster). It achieves this by noting
that a large proportion of participants hold much of the stake; for these participants we
can assign them a deterministic seat in the committee (no lottery over stake needed).
The remaining seats are then handled using random sampling over stake (like Praos with a VRF).
The result of this is that these fixed seats have zero variance (since the VRF approach is stochastic).
This zero variance is beneficial as it concentrates the voting weight tightly around the true
staking distribution, which reduces the number of pools that need to participate to reach consensus.

In a nutshell, with FA; you know a pool can vote, while with a randomness based election model there is
a chance it can vote.

This is why the paper is called

> fait accompli /fĕt″ ä-kŏm-plē′, fāt″/ noun An accomplished, presumably irreversible deed or fact.

Since a large proportion of the committee is fixed in advance.

Implementation notes:
- The paper uses 1-based indexing for party indices (p₁, p₂, ..., pᵢ*)
- This implementation uses Haskell's 0-based indexing
- Paper's i* corresponds to (iStar + 1) when converting between the two conventions
- Therefore, n₁ = i* - 1 = iStar persistent seats are assigned
-}

-- | The function that calculates the sum of the stake of the remaining
-- elements from index `i` onwards. Defined in the paper in figure 6 on page 854.
rho :: Int -> OrderedSetOfParties -> RelativeStake
rho i = sum . drop i . map stake . parties

-- | Find the smallest `i` such that for `partyList' = drop i partyList` either `rho i partyList = 0`
-- or
--
--      `(1-\frac{partyList' !! i }{rho i partyList'})^2 >= \frac{n-i}{n-i+1}`
--
-- Note that due to this function ordering the inputs of the list
-- from large to small stake, the first check is equivalent to having
-- the remaining stake in `partyList'` being zero.
findIStar :: OrderedSetOfParties -> Int
findIStar (OrderedSetOfParties [] _) = 0
findIStar osp@(OrderedSetOfParties partyList n) = go 0 partyList
  where
    n' = fromIntegral @CommitteeSize @RelativeStake n
    -- This case is never reached, added to make the function total to be sure.
    go i [] = i
    go i (x : xs)
      -- Note that if `i >= n`, we have reached the target committee size
      | i' >= n' = i
      -- Note that the second condition here in the or statement
      -- reduces to "either $\rho_i=0$" in the paper. This is because
      -- the input list is ordered. So, $\rho_i=0" <=> all remaining
      -- stake of the trailing elements in the list is zero <=>
      -- the first element of the remaining list has zero stake.
      | stake x == 0 = i
      -- note that this differs from the paper, as we used 0 based indexes
      | (1 - f) * (1 - f) >= (n' - i' - 1) / (n' - i') = i
      | otherwise = go (i + 1) xs
      where
        -- If `snd x > 0` then `rho i (OrderedSetOfParties (x:xs)) > 0`
        -- hence f below is well-defined.
        --
        -- NOTE:
        -- This is currently O(n^2). See `findIStarAcc` for an
        -- accumulator-based O(n) implementation.
        f = stake x / rho i osp
        i' = fromIntegral @Int @RelativeStake i

-- | Accumulator-based `findIStar` running in O(n).
findIStarAcc :: OrderedSetOfParties -> Int
findIStarAcc (OrderedSetOfParties [] _) = 0
findIStarAcc (OrderedSetOfParties partyList n) = go 0 partyList totalStake
  where
    totalStake = sum (map stake partyList)
    n' = fromIntegral @CommitteeSize @RelativeStake n

    -- This case is never reached, added to make the function total to be sure.
    go i [] _ = i
    go i (x : xs) remainingStake
      -- Note that if `i >= n`, we have reached the target committee size
      | i' >= n' = i
      | stake x == 0 = i
      | (1 - f) * (1 - f) >= (n' - i' - 1) / (n' - i') = i
      | otherwise = go (i + 1) xs (remainingStake - stake x)
      where
        -- If `stake x > 0` then `remainingStake > 0` hence `f` is well-defined.
        f = stake x / remainingStake
        i' = fromIntegral @Int @RelativeStake i

-- | A type representing an epoch-specific pool index
type PersistentVoterIndex = Word16

-- | A type representing a persistent voter seat. The goal of fait accompli
-- is to assign more weight to large stake pools
data PersistentSeat = PersistentSeat
  { publicVoteKeyPersistent :: PublicKeyLeios 'Vote
  , weightPersistentSeat :: Weight
  }
  deriving (Show)

type PersistentSeats = Map.Map PersistentVoterIndex PersistentSeat

findPersistentSeatByPublicKey ::
  PublicKeyLeios 'Vote ->
  PersistentSeats ->
  Maybe (PersistentVoterIndex, PersistentSeat)
findPersistentSeatByPublicKey pk =
  find (\(_, v) -> publicVoteKeyPersistent v == pk) . Map.toList

data NonPersistentVoter = NonPersistentVoter
  { publicVoteKeyNonPersistent :: PublicKeyLeios 'Vote
  , stakeNonPersistentVoter :: RelativeStake
  }
  deriving (Show)

type NonPersistentVoters = Map.Map PoolId NonPersistentVoter

findNonPersistentVoterByPublicKey ::
  PublicKeyLeios 'Vote ->
  NonPersistentVoters ->
  Maybe (PoolId, NonPersistentVoter)
findNonPersistentVoterByPublicKey pk =
  find (\(_, v) -> publicVoteKeyNonPersistent v == pk) . Map.toList

data NonPersistentLocalSortition = NonPersistentLocalSortition
  { voters :: NonPersistentVoters
  , weightPerNonPersistentSeat :: Weight
  }
  deriving (Show)

-- | The per-epoch selected committee for Leios
data CommitteeSelection = CommitteeSelection
  { persistentSeats :: PersistentSeats
  , nonPersistentVoters :: NonPersistentLocalSortition
  , praosNonce :: PraosNonce
  }
  deriving (Show)

wFA :: PraosNonce -> OrderedSetOfParties -> CommitteeSelection
wFA nonce osp@(OrderedSetOfParties prts n) =
  CommitteeSelection
    { persistentSeats = persSeats
    , nonPersistentVoters = nonPersVoters
    , praosNonce = nonce
    }
  where
    iStar = findIStarAcc osp
    -- Note on indexing: The paper uses 1-based indexing where i* is the party index,
    -- while this implementation uses 0-based Haskell indexing. Therefore:
    --   - Paper's i* (1-based party number) = Haskell's iStar + 1
    --   - Paper's n₁ = i* - 1 (persistent seats) = Haskell's iStar
    -- We assign persistent seats to parties at indices [0..iStar-1] (iStar parties total).
    (persistent, nonPersistent) = splitAt iStar prts
    n2 =
      fromIntegral @CommitteeSize @RelativeStake n - fromIntegral @Int @RelativeStake (length persistent)

    persSeats :: PersistentSeats
    persSeats =
      Map.fromList $
        [ ( fromIntegral @Int @Word16 idx
          , PersistentSeat
              { publicVoteKeyPersistent = publicVoteKey p
              , weightPersistentSeat = stake p
              }
          )
        | (idx, p) <- zip [(0 :: Int) ..] persistent
        ]

    nonPersVoters :: NonPersistentLocalSortition
    nonPersVoters =
      NonPersistentLocalSortition
        { voters =
            Map.fromList
              [ ( poolId p
                , NonPersistentVoter
                    { publicVoteKeyNonPersistent = publicVoteKey p
                    , stakeNonPersistentVoter = stake p
                    }
                )
              | p <- nonPersistent
              ]
        , weightPerNonPersistentSeat = if n2 /= 0 then remStake / n2 else 0
        }

    remStake :: RelativeStake
    remStake = rho 0 $ OrderedSetOfParties nonPersistent n
