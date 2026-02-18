module Cardano.Leios.LocalSortition where

-- Goal of this module should be to pre-calc
--
-- Map NonPersistentVoter ThresholdsNonPersistentVoter
--
-- where `ThresholdsNonPersistentVoter` is some list of (k, abstract Taylor expansion type)
--
-- so that, when we get a vote, we can check for each k, given a threshold, if it is below that order k
-- And given the first occurrence where it is not, return `k-1` seats (if 0, no seats)

type Threshold = Rational

-- calculateThreshold :: NonPersistentVoter -> Threshold
-- calculateThreshold NonPersistentVoter{stakeNonPersistentVoter = stake} =
