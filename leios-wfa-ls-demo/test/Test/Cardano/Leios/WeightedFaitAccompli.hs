{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Leios.WeightedFaitAccompli (tests, genOrderedSetOfParties) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Api.Shelley (makePraosNonce)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (KeyHash (..))
import Cardano.Leios.Committee
import Cardano.Leios.Utils (toVerKeyForBLS)
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  findIStar,
  findIStarAcc,
  stakeNonPersistentVoter,
  wFA,
 )
import qualified Data.ByteString.Char8 as BSC
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Data.Ratio ((%))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  QuickCheckTests (..),
  chooseInt,
  chooseInteger,
  counterexample,
  forAllBlind,
  testProperty,
  vectorOf,
 )

tests :: TestTree
tests =
  testGroup
    "WeightedFaitAccompli"
    [ localOption (QuickCheckTests 1000) $
        testProperty
          "findIStar matches accumulator version"
          prop_findIStarAccMatches
    , localOption (QuickCheckTests 1000) $
        testProperty
          "non-persistent voters relative stakes sum to 1"
          prop_nonPersistentStakesSumToOne
    ]

-- | Property test verifying that the O(n^2) and O(n) implementations of
-- findIStar produce identical results for all valid inputs.
prop_findIStarAccMatches :: Property
prop_findIStarAccMatches =
  forAllBlind genOrderedSetOfParties $ \osp ->
    findIStar osp == findIStarAcc osp

-- | Generator for OrderedSetOfParties with random stake distributions.
-- Strategy:
--   - Generate 0-300 pools with positive rational stakes
--   - Normalize stakes to sum to 1 (representing total stake distribution)
--   - Sort parties by descending stake (as required by the algorithm)
--   - Choose a committee size between 0 and the number of pools
--   - Create parties with unique PoolIds (using index as seed)
genOrderedSetOfParties :: Gen OrderedSetOfParties
genOrderedSetOfParties = do
  numPools <- chooseInt (0, 300)
  stakes <- if numPools == 0 then pure [] else vectorOf numPools genStakePositive
  let totalStake = sum stakes
      normalizedStakes =
        if totalStake == 0
          then stakes
          else map (/ totalStake) stakes
      sortedStakes = sortOn Down normalizedStakes
      -- Create parties with unique pool IDs (using the index as the pool ID seed)
      partyList = zipWith mkParty [0 ..] sortedStakes
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

-- | Helper to construct a Party with the given stake and a unique PoolId.
-- The publicVoteKey is derived from the poolId using the BLS key derivation.
mkParty :: Int -> Rational -> Party
mkParty idx stk =
  let pId = mkDummyPoolId idx
   in Party
        { poolId = pId
        , publicVoteKey = toVerKeyForBLS pId testNetworkId
        , stake = stk
        }

-- | Test network ID for generating keys.
testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

-- | Property test verifying that the sum of all non-persistent voter stakes
-- equals exactly 1 after normalization by wFA. This validates that the normalization
-- logic in wFA correctly divides each stake by the remaining stake.
prop_nonPersistentStakesSumToOne :: Property
prop_nonPersistentStakesSumToOne =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        nonPersVoters = voters $ nonPersistentVoters committee
        stakesSum = sum $ map stakeNonPersistentVoter $ Map.elems nonPersVoters
        -- Handle edge case where all voters are persistent (no non-persistent voters to normalize)
        isEmpty = Map.null nonPersVoters
     in counterexample
          ( "Sum of non-persistent stakes: "
              ++ show stakesSum
              ++ "\nNumber of non-persistent voters: "
              ++ show (Map.size nonPersVoters)
              ++ "\nCommittee size: "
              ++ show (committeeSize osp)
              ++ "\nNumber of parties: "
              ++ show (length $ parties osp)
          )
          -- Pass if either: (1) no non-persistent voters exist, or (2) their stakes sum to exactly 1
          $ isEmpty || stakesSum == 1

-- | Create a dummy but valid PoolId from an integer index.
-- This ensures each party has a unique pool ID.
-- Different indices will produce different hashes regardless of input size.
mkDummyPoolId :: Int -> PoolId
mkDummyPoolId idx =
  let
    -- Hash the index to create a Blake2b_224 hash (28 bytes output)
    h = Hash.castHash $ Hash.hashWith id $ BSC.pack (show idx)
   in
    KeyHash h
