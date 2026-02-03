{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WeightedFaitAccompli (
    CommitteeSize,
    OrderedSetOfParties,
    mkOrderedSetOfParties,
    appointSeats,
) where

import Cardano.Api.Ledger (KeyHash)
import Cardano.Crypto.VRF (VerKeyVRF)
import Cardano.Ledger.Hashes (HASH, Hash)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Protocol.Crypto (VRF)
import Data.List (sortOn)
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

{- | A type representing the ordered (from large to small) set of
stakepools. To construct this use `mkOrderedSetOfParties` to ensure
the underlying list is ordered.
-}
newtype OrderedSetOfParties = OrderedSetOfParties
    { unOrderedSetOfParties :: [(KeyHash StakePool, Rational)]
    }

{- | A function to safely construct a `OrderedSetOfParties` from a
list of stakepools identified by their `KeyHash StakePool` and
their stake `Rational`.
-}
mkOrderedSetOfParties :: [(KeyHash StakePool, Rational)] -> OrderedSetOfParties
mkOrderedSetOfParties = OrderedSetOfParties . sortOn (Down . snd)

{- | The function that calculated the sum of the stake of the remaining
`i` elements of the input. Defiend in the paper in figure 6 on page 854.
-}
rho :: Int -> OrderedSetOfParties -> Rational
rho i = sum . drop i . map snd . unOrderedSetOfParties

{- | Find the smallest `i` such that for `lst' = drop i lst`  either `rho i lst = 0`
or

     `(1-\frac{lst' !! i }{rho i lst'})^2 >= \frac{n-i}{n-i+1}`

Note that due to this function ordering the inputs of the list
from large to small stake, the first check is equivalent to having
the remaining stake in `lst'` being zero.
-}
findIStar :: CommitteeSize -> OrderedSetOfParties -> Int
findIStar _ (OrderedSetOfParties []) = 0
findIStar n (OrderedSetOfParties lst) = go 0 lst
  where
    go i (x : xs)
        -- Note that the second condition here in the or statement
        -- is a reduction "either $\rho_i=0$" in the paper. This because
        -- the input list is ordered. So, $\rho_i=0" <=> all remaining
        -- stake of the trailing elements in the list is zero <=>
        -- the first element of the remaing list has zero stake.
        | (1 - f) ^ 2 >= (n' - i') / (n' - i' + 1) || snd x == 0 = i
        | otherwise = go (i + 1) xs
      where
        f = snd x / rho i (OrderedSetOfParties (x : xs))
        n' = fromIntegral @CommitteeSize @Rational n
        i' = fromIntegral @Int @Rational i

{- | A type to represent a persistent seat.

TODO: note that the paper does not care about the seatId
for now keeping it for housekeeping and logging
-}
data PersistentSeat = PersistentSeat
    { seatPool :: KeyHash StakePool
    , seatId :: Word16
    , weight :: Rational
    }
    deriving (Show)

type PersistentSeats = [PersistentSeat]

type RemainderOfOrderedSetOfParties = OrderedSetOfParties

-- data NonPersistentSeat = NonPersistentSeat
--   {
--   }
-- type LocalSortitionSeat =

appointSeats :: CommitteeSize -> OrderedSetOfParties -> PersistentSeats
appointSeats n (OrderedSetOfParties list) =
    take iStar $ zipWith mkSeat [0 ..] list
  where
    mkSeat :: Word16 -> (KeyHash StakePool, Rational) -> PersistentSeat
    mkSeat sid (pool, w) =
        PersistentSeat{seatPool = pool, seatId = sid, weight = w}
    iStar = findIStar n $ OrderedSetOfParties list
