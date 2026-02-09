{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentVoters (..),
  PersistentSeats,
  PersistentSeat (..),
  PersistentVoterId,
  wFA,
  rho,
) where

import Cardano.Leios.Committee
import Cardano.Leios.Crypto
import qualified Data.Map as Map
import Data.Word (Word16)

{-
This module implements the core of Weighted Fait Accompli.

The code below closely follows the paper found here: https://dl.acm.org/doi/10.1145/3576915.3623194

The why:

The core idea of the above paper is that one can reduce randomness where it isn’t needed
to get a smaller committee (so that hopefully consensus is reaches faster). It achieves this by noting
that large proportion of participants holds much of the stake, for these participants we
can assign them a deterministic seat in the commitee (no lottery over stake needed).
This remaining seats are then handles using random sampling over stake (like Praos with a VRF).
The result of this is that these fixed seats have zero variance (since the VRF approach is stochastic).
This zero variance is beneficial as it concentrates the voting weight teigtly around the true
staking distribution, which reduces the number of pools that need to participate to reach consensus.

In a nutshell, with FA; you know a pool can vote, while with a randomness based election model there is
a chance it can vote.

This is why the paper is called

> fait accompli /fĕt″ ä-kŏm-plē′, fāt″/ noun An accomplished, presumably irreversible deed or fact.

Since a large proportion of the committee is fixed in advanced.
-}

-- | The function that calculated the sum of the stake of the remaining
-- `i` elements of the input. Defined in the paper in figure 6 on page 854.
rho :: Int -> OrderedSetOfParties -> Rational
rho i = sum . drop i . map stake . parties

-- | Find the smallest `i` such that for `lst' = drop i lst`  either `rho i lst = 0`
-- or
--
--      `(1-\frac{lst' !! i }{rho i lst'})^2 >= \frac{n-i}{n-i+1}`
--
-- Note that due to this function ordering the inputs of the list
-- from large to small stake, the first check is equivalent to having
-- the remaining stake in `lst'` being zero.
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

-- | A type representing an Epoch-specific pool identifier
-- Note that the CIP 164 CDDL does not say what this should be!
type PersistentVoterId = Word16

-- | A type representing a persistent voter seat. The goal of fait accompli
-- is to assign more weight to large stakepools
data PersistentSeat = PersistentSeat
  { publicVoteKeyPersistent :: PublicVoteKey
  , weightPersistentSeat :: Rational
  }
  deriving (Show)

type PersistentSeats = Map.Map PersistentVoterId PersistentSeat

data NonPersistentVoters = NonPersistentVoters
  { voters :: Map.Map PoolID PublicVoteKey
  , weightPerNonPersistentSeat :: Rational
  }
  deriving (Show)

data CommitteeSelection = CommitteeSelection
  { persistentSeats :: PersistentSeats
  , nonPersistentVoters :: NonPersistentVoters
  }
  deriving (Show)

wFA :: OrderedSetOfParties -> CommitteeSelection
wFA osp@(OrderedSetOfParties prts n) =
  CommitteeSelection
    { persistentSeats = persSeats
    , nonPersistentVoters = nonPersVoters
    }
  where
    iStar = findIStar osp

    -- Note that the paper appoints pools `i \in [0, .. (i*-1)]
    -- to be persistent voters
    i = max 0 $ iStar - 1
    (persistent, nonPersistent) = splitAt i prts
    n2 = fromIntegral @CommitteeSize @Rational n - fromIntegral @Int @Rational (length persistent)

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

    nonPersVoters :: NonPersistentVoters
    nonPersVoters =
      NonPersistentVoters
        { voters =
            Map.fromList
              [ (poolID p, publicVoteKey p)
              | p <- nonPersistent
              ]
        , weightPerNonPersistentSeat = if n2 /= 0 then remStake / n2 else 0
        }

    remStake :: Rational
    remStake = rho 0 $ OrderedSetOfParties nonPersistent n
