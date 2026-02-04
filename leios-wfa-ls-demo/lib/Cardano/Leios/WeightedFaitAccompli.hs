{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.WeightedFaitAccompli (
    CommitteeSize,
    Party (..),
    OrderedSetOfParties,
    mkOrderedSetOfParties,
    wFALS,
    -- appointSeats,
) where

import Cardano.Api.Ledger (KeyHash)
import Cardano.Crypto.DSIGN
import Cardano.Ledger.Keys (KeyRole (..))
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Data.Word (Word16)

{-
This module implements the core of Weighted Fait Accompli with local sortition.

The code below closely follows the paper found here: https://dl.acm.org/doi/10.1145/3576915.3623194

Note that the protocol described in figure 6 on page 854 assumes an ordered
list of stakepools and their stake in the construction of the committee.

-}

{- | A type wrapper around `Word16` representing the maximal committee
the protocol targets. Note that this the protocol on avarage will
have a committee size of this size, since the non-persistent seats
are stochasticly assigned based on a VRF.
-}
type CommitteeSize = Word16

data Party = Party
    { poolId :: KeyHash StakePool
    , blsVerKey :: VerKeyDSIGN BLS12381MinSigDSIGN
    , stake :: Rational
    }
    deriving (Show)

{- | A type representing the ordered (from large to small) set of
stakepools. To construct this use `mkOrderedSetOfParties` to ensure
the underlying list is ordered. This type also contains the committee
size.
-}
data OrderedSetOfParties = OrderedSetOfParties
    { parties :: [Party]
    , committeeSize :: CommitteeSize
    }

data MkOrderedSetOfPartiesError
    = CommitteeTooLarge
        { requestedCommitteeSize :: CommitteeSize
        , numberOfPools :: Int
        }
    | TooManyPools
        { numberOfPools :: Int
        , maxPoolsAllowed :: Word16
        }
    | TotalStakeNotOne
    deriving (Show)

{- | A function to safely construct a `OrderedSetOfParties` from a
list of stakepools identified by their `KeyHash StakePool`,
their stake `Rational` and the `committeeSize`.

Note that we require the committee size to be smaller or equal
to the number of pools. We further require that the number of
pools is never more than the `maxBound` of an `Word16`. And lastly
that the total sum of the stake is one. Perhaps we could also check
for negative stake, but I think we can assume that the node returns that.
-}
mkOrderedSetOfParties :: CommitteeSize -> [Party] -> Either MkOrderedSetOfPartiesError OrderedSetOfParties
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
    | (sum . map stake) poolDistr /= 1 = Left TotalStakeNotOne
    | otherwise =
        Right $
            OrderedSetOfParties
                { parties = sortOn (Down . stake) poolDistr
                , committeeSize = comSize
                }
  where
    numPools = length poolDistr

{- | The function that calculated the sum of the stake of the remaining
`i` elements of the input. Defined in the paper in figure 6 on page 854.
-}
rho :: Int -> OrderedSetOfParties -> Rational
rho i = sum . drop i . map stake . parties

{- | Find the smallest `i` such that for `lst' = drop i lst`  either `rho i lst = 0`
or

     `(1-\frac{lst' !! i }{rho i lst'})^2 >= \frac{n-i}{n-i+1}`

Note that due to this function ordering the inputs of the list
from large to small stake, the first check is equivalent to having
the remaining stake in `lst'` being zero.
-}
findIStar :: OrderedSetOfParties -> Int
findIStar (OrderedSetOfParties [] _) = 0
findIStar (OrderedSetOfParties lst n) = go 0 lst
  where
    -- This case is never reached, added to make the function total to be sure.
    go i [] = i
    go i (x : xs)
        -- Note that the second condition here in the or statement
        -- is a reduction "either $\rho_i=0$" in the paper. This because
        -- the input list is ordered. So, $\rho_i=0" <=> all remaining
        -- stake of the trailing elements in the list is zero <=>
        -- the first element of the remaing list has zero stake.
        | stake x == 0 = i
        | (1 - f) * (1 - f) >= (n' - i') / (n' - i' + 1) = i
        | otherwise = go (i + 1) xs
      where
        -- If `snd x > 0` then `rho i (OrderedSetOfParties (x:xs)) > 0`
        -- hence f is below is well-defined.
        --
        -- TODO ---------
        -- This is currently O(n^2), better to use an
        -- accumulator and pass the total along minus `snd x`
        -- to prevent recalculating the remaining stake again and again.
        -- Keeping it to semanticly match the paper for now
        f = stake x / rho i (OrderedSetOfParties (x : xs) n)
        -- note that for `i == n`, we have that
        -- `(1-f)^2` is trivially bigger or equal than zero.
        -- This implies that the case `n' + 1 == i'`
        -- is never reached and thus the divisor is never zero.
        n' = fromIntegral @CommitteeSize @Rational n
        i' = fromIntegral @Int @Rational i

{- | A type representing an Epoch-specific pool identifier
Note that the CIP 164 CDDL does not say what this should be!
-}
type PersistentVoterId = Word16

data PersistentSeat = PersistentSeat
    { blsVK :: VerKeyDSIGN BLS12381MinSigDSIGN
    , weight :: Rational
    }
    deriving (Show)

type PersistentSeats = Map.Map PersistentVoterId PersistentSeat

type NonPersistentVoters = Map.Map (KeyHash StakePool) (VerKeyDSIGN BLS12381MinSigDSIGN)

type RemainingNonPersistentStake = Rational

data EpochData = EpochData
    { persistentSeats :: PersistentSeats
    , nonPersistentVoters :: NonPersistentVoters
    , remainingStake :: RemainingNonPersistentStake
    }
    deriving (Show)

wFALS :: OrderedSetOfParties -> EpochData
wFALS osp@(OrderedSetOfParties prts n) =
    EpochData
        { persistentSeats = persSeats
        , nonPersistentVoters = nonPersVoters
        , remainingStake = remStake
        }
  where
    iStar = findIStar osp
    -- Note that the paper appoints pools `i \in [0, .. (i*-1)]
    -- to be persistent voters
    i = max 0 $ iStar - 1
    (pers, nonPers) = splitAt i prts
    persSeats :: PersistentSeats
    persSeats =
        Map.fromList $
            [ ( fromIntegral idx
              , PersistentSeat
                    { blsVK = blsVerKey p
                    , weight = stake p
                    }
              )
            | (idx, p) <- zip [(0 :: Int) ..] pers
            ]

    nonPersVoters :: NonPersistentVoters
    nonPersVoters =
        Map.fromList
            [ (poolId p, blsVerKey p)
            | p <- nonPers
            ]

    remStake :: RemainingNonPersistentStake
    remStake = rho i $ OrderedSetOfParties nonPers n
