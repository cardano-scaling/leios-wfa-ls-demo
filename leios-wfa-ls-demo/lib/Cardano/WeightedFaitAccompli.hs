{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WeightedFaitAccompli (
    findIStar,
) where

import Cardano.Api.Ledger (KeyHash)
import Cardano.Crypto.VRF (VerKeyVRF)
import Cardano.Ledger.Hashes (HASH, Hash)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Protocol.Crypto (VRF)
import Data.List (sortOn)
import Data.Ord (Down (..))

rho :: Int -> [(KeyHash StakePool, Rational)] -> Rational
rho i = sum . drop i . map snd

-- Find the smallest i such that for lst' = drop i lst  either `rho i lst = 0`
-- or
--      (1-\frac{lst' !! i }{rho i lst'})^2 >= \frac{n-i}{n-i+1}
--
-- Note that due to this function ordering the inputs of the list
-- from large to small stake, the first check is equivalent to having
-- the remaining stake in `lst'` being zero.
findIStar :: Int -> [(KeyHash StakePool, Rational)] -> Int
findIStar i = findIStarUnsafe i . sortOn (Down . snd)

-- This function expects an ordered list of stake
findIStarUnsafe :: Int -> [(KeyHash StakePool, Rational)] -> Int
findIStarUnsafe _ [] = 0
findIStarUnsafe n lst = go 0 lst
  where
    go i (x : xs)
        | (1 - f) ^ 2 >= (n' - i') / (n' - i' + 1) || snd x == 0 = i
        | otherwise = go (i + 1) xs
      where
        f = snd x / rho i (x : xs)
        n' = fromIntegral @Int @Rational n
        i' = fromIntegral @Int @Rational i
