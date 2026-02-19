{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Leios.Committee
import Cardano.Leios.Crypto (KeyRoleLeios (..), PublicKeyLeios)
import Cardano.Leios.WeightedFaitAccompli (findIStar, findIStarAcc)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Ratio ((%))
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  QuickCheckTests (..),
  chooseInt,
  chooseInteger,
  forAllBlind,
  testProperty,
  vectorOf,
 )

main :: IO ()
main =
  defaultMain $
    testGroup
      "WeightedFaitAccompli"
      [ localOption (QuickCheckTests 1000) $
          testProperty
            "findIStar matches accumulator version"
            prop_findIStarAccMatches
      ]

-- | Property test verifying that the O(n^2) and O(n) implementations of
-- findIStar produce identical results for all valid inputs.
prop_findIStarAccMatches :: Property
prop_findIStarAccMatches =
  forAllBlind genOrderedSetOfParties $ \osp ->
    findIStar osp == findIStarAcc osp

-- | Generator for OrderedSetOfParties with random stake distributions.
-- Strategy:
--   - Generate 0-200 pools with positive rational stakes
--   - Normalize stakes to sum to 1 (representing total stake distribution)
--   - Sort parties by descending stake (as required by the algorithm)
--   - Choose a committee size between 0 and the number of pools
genOrderedSetOfParties :: Gen OrderedSetOfParties
genOrderedSetOfParties = do
  numPools <- chooseInt (0, 800)
  stakes <- if numPools == 0 then pure [] else vectorOf numPools genStakePositive
  let totalStake = sum stakes
      normalizedStakes =
        if totalStake == 0
          then stakes
          else map (/ totalStake) stakes
      sortedStakes = sortOn Down normalizedStakes
      partyList = map mkParty sortedStakes
  comSize <- if numPools == 0 then pure 0 else chooseInt (0, numPools)
  pure
    OrderedSetOfParties
      { parties = partyList
      , committeeSize = fromIntegral @Int @CommitteeSize comSize
      }

-- | Generator for positive rational stake values.
-- Generates rationals with numerator and denominator between 1 and 1000.
genStakePositive :: Gen Rational
genStakePositive = do
  numerator <- chooseInteger (1, 1000)
  denominator <- chooseInteger (1, 1000)
  pure (numerator % denominator)

-- | Helper to construct a Party with the given stake.
-- The poolId and publicVoteKey fields are set to dummy values since
-- findIStar and findIStarAcc only depend on the stake field.
mkParty :: Rational -> Party
mkParty stk =
  Party
    { poolId = dummyPoolId
    , publicVoteKey = dummyPublicKey
    , stake = stk
    }

-- | Dummy pool ID for testing. Safe to use because findIStar/findIStarAcc
-- only examine the stake field of Party and never access poolId.
dummyPoolId :: PoolId
dummyPoolId = error "dummyPoolId: not used in findIStar tests"

-- | Dummy public key for testing. Safe to use because findIStar/findIStarAcc
-- only examine the stake field of Party and never access publicVoteKey.
dummyPublicKey :: PublicKeyLeios 'Vote
dummyPublicKey = error "dummyPublicKey: not used in findIStar tests"
