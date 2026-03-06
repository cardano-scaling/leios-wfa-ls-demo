{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Leios.LocalSortition (tests) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Api.Shelley (makePraosNonce)
import Cardano.Leios.Committee
import Cardano.Leios.Crypto (
  KeyRoleLeios (..),
  OutputVRF,
  PrivateKeyLeios (..),
  coercePrivateKeyLeios,
  signWithRoleLeios,
 )
import Cardano.Leios.LocalSortition (checkLeaderValueLeios)
import Cardano.Leios.NonIntegral (FirstNonLowerError (..))
import Cardano.Leios.Utils (toSkForBLS)
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  stakeNonPersistentVoter,
  wFA,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Test.Cardano.Leios.WeightedFaitAccompli (genOrderedSetOfParties)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Property,
  QuickCheckTests (..),
  counterexample,
  forAllBlind,
  testProperty,
 )

tests :: TestTree
tests =
  testGroup
    "LocalSortition"
    [ localOption (QuickCheckTests 1000) $
        testProperty
          "checkLeaderValueLeios never returns FirstNonLowerError for wFA selected committees"
          prop_localSortitionNoError
    ]

-- | Property test verifying that when wFA selects a committee, performing local
-- sortition on the non-persistent voters never produces a FirstNonLowerError.
--
-- This test:
-- 1. Generates a random committee using wFA
-- 2. For each non-persistent voter, simulates local sortition
-- 3. Verifies that checkLeaderValueLeios always returns Right (number of seats won)
prop_localSortitionNoError :: Property
prop_localSortitionNoError =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        nonPersVoters = voters $ nonPersistentVoters committee
        -- Calculate n2: the number of non-persistent seats
        -- n2 = target committee size - number of persistent voters
        n2 =
          fromIntegral @CommitteeSize @Integer (committeeSize osp)
            - fromIntegral @Int @Integer (Map.size (persistentSeats committee))
        -- For each non-persistent voter, check local sortition
        results = Map.mapWithKey (checkVoterSortition n2) nonPersVoters
        -- Find any errors
        errors = Map.filter isLeft results
     in counterexample
          ( "Committee size: "
              ++ show (committeeSize osp)
              ++ "\nNumber of parties: "
              ++ show (length $ parties osp)
              ++ "\nNumber of non-persistent voters: "
              ++ show (Map.size nonPersVoters)
              ++ "\nn2 (non-persistent seats): "
              ++ show n2
              ++ "\nErrors found: "
              ++ show (Map.size errors)
              ++ "\nError details: "
              ++ show errors
          )
          $ Map.null errors
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False

-- | Simulate local sortition for a single non-persistent voter
checkVoterSortition ::
  Integer -> -- n2: number of non-persistent seats
  PoolId ->
  NonPersistentVoter ->
  Either FirstNonLowerError Int
checkVoterSortition n2 pId voter =
  checkLeaderValueLeios vrfOutput σ n2
  where
    -- Generate a VRF output for this pool (using the pool ID as the message)
    vrfOutput = generateVRFOutput pId testNetworkId
    -- Use the normalized stake from the voter
    σ = stakeNonPersistentVoter voter

-- | Generate a VRF output for testing purposes
-- This simulates a pool signing a message with their VRF key
generateVRFOutput :: PoolId -> NetworkId -> OutputVRF
generateVRFOutput pId nId =
  let sk = toSkForBLS pId
      vrfKey = coercePrivateKeyLeios @'Vote @'VRF (PrivateKeyLeios (nId, sk))
      -- Use a simple message for testing
      msg :: ByteString
      msg = "local-sortition-test"
   in signWithRoleLeios msg vrfKey

-- | Test network ID for generating keys
testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)
