{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Leios.Vote (tests) where

import Cardano.Api (NetworkId (..), NetworkMagic (..), PraosNonce)
import Cardano.Api.Shelley (makePraosNonce)
import Cardano.Binary (decodeFull, serialize')
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Leios.Committee (
  OrderedSetOfParties (..),
  Party (..),
 )
import Cardano.Leios.Crypto (
  KeyRoleLeios (..),
  PrivateKeyLeios (..),
 )
import Cardano.Leios.Types (
  ElectionId,
  EndorserBlockHash,
  PoolId,
 )
import Cardano.Leios.Utils (toSkForBLS)
import Cardano.Leios.Vote (
  LeiosVote (..),
  NonPersistentVote (..),
  PersistentVote (..),
  createNonPersistentVote,
  createPersistentVote,
  verifyLeiosVote,
 )
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  PersistentSeat (..),
  findPersistentSeatByPublicKey,
  wFA,
  weightPersistentSeat,
 )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Either (lefts, rights)
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
    "Vote"
    [ localOption (QuickCheckTests 1000) $
        testProperty
          "persistent vote roundtrip"
          prop_persistentVoteRoundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "non-persistent vote roundtrip"
          prop_nonPersistentVoteRoundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "persistent vote CBOR roundtrip"
          prop_persistentVoteCBORRoundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "non-persistent vote CBOR roundtrip"
          prop_nonPersistentVoteCBORRoundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "leios vote CBOR roundtrip"
          prop_leiosVoteCBORRoundtrip
    ]

-- | Property test verifying that persistent votes can be created and verified
-- in a roundtrip fashion. This test:
-- 1. Generates a random committee using wFA
-- 2. For each party, attempts to create a persistent vote (only succeeds if party has a seat)
-- 3. If vote creation succeeds, verifies the vote and checks weight matches the seat weight
-- 4. Test passes if all created persistent votes verify successfully
prop_persistentVoteRoundtrip :: Property
prop_persistentVoteRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        persistentVoters = persistentSeats committee
        eId = testElectionId
        ebHash = testEndorserBlockHash
        -- Test each party to see if it has a persistent seat
        results = map (testPersistentVoterForParty committee eId ebHash) (parties osp)
        -- Filter to only parties with persistent seats and collect any failures
        failures = lefts results
     in counterexample
          ( "Committee size: "
              ++ show (committeeSize osp)
              ++ "\nNumber of parties: "
              ++ show (length $ parties osp)
              ++ "\nNumber of persistent voters: "
              ++ show (Map.size persistentVoters)
              ++ "\nFailures found: "
              ++ show (length failures)
              ++ "\nFailure details: "
              ++ show failures
          )
          $ null failures

-- | Property test verifying that non-persistent votes can be created and verified
-- in a roundtrip fashion. This test:
-- 1. Generates a random committee using wFA
-- 2. For each non-persistent voter in the committee, attempts to create a vote
-- 3. Vote creation may fail if VRF threshold not met (this is EXPECTED behavior)
-- 4. If vote creation succeeds, verifies the vote
-- 5. Test passes if all successfully created votes also verify successfully
--    (creation failures are OK, but verification failures are NOT)
prop_nonPersistentVoteRoundtrip :: Property
prop_nonPersistentVoteRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        nonPersistentVotersData = voters $ nonPersistentVoters committee
        eId = testElectionId
        ebHash = testEndorserBlockHash
        -- For each non-persistent voter, test vote creation and verification
        -- Note: some votes may fail to create due to VRF threshold
        results = Map.mapWithKey (testNonPersistentVoter nonce committee eId ebHash) nonPersistentVotersData
        -- Find verification failures (only count Left values, not creation failures)
        verificationFailures = lefts (Map.elems results)
     in counterexample
          ( "Committee size: "
              ++ show (committeeSize osp)
              ++ "\nNumber of parties: "
              ++ show (length $ parties osp)
              ++ "\nNumber of non-persistent voters: "
              ++ show (Map.size nonPersistentVotersData)
              ++ "\nVerification failures found: "
              ++ show (length verificationFailures)
              ++ "\nFailure details: "
              ++ show verificationFailures
          )
          $ null verificationFailures

-- | Test a party to see if it has a persistent seat, and if so, test vote creation and verification
testPersistentVoterForParty ::
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  Party ->
  Either String ()
testPersistentVoterForParty committee eId ebHash party = do
  let privKey = makePrivateKey (poolId party)
      pk = publicVoteKey party
  -- Try to create a persistent vote - this will fail if the party doesn't have a persistent seat
  case createPersistentVote committee privKey eId ebHash of
    Left _ -> Right () -- Not a persistent voter, skip
    Right pVote -> do
      -- Verify the vote
      actualWeight <- verifyLeiosVote committee eId ebHash (LeiosPersistentVote pVote)
      -- Find the expected weight by looking up the seat
      case findPersistentSeatByPublicKey pk (persistentSeats committee) of
        Nothing -> Left "Persistent vote created but no seat found"
        Just (_, seat) ->
          let expectedWeight = weightPersistentSeat seat
           in if actualWeight == expectedWeight
                then Right ()
                else Left $ "Weight mismatch: expected " ++ show expectedWeight ++ ", got " ++ show actualWeight

-- | Test a single non-persistent voter: create and verify their vote
-- Returns Left msg for verification failures (unexpected)
-- Returns Right () for success or creation failures (VRF threshold not met, which is expected)
testNonPersistentVoter ::
  PraosNonce ->
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  PoolId ->
  npv ->
  Either String ()
testNonPersistentVoter nonce committee eId ebHash pId _voter =
  let privKey = makePrivateKey pId
   in case createNonPersistentVote nonce committee privKey eId ebHash of
        -- Creation failure due to VRF threshold is expected, not an error
        Left _err -> Right ()
        Right npVote ->
          -- Verify the vote - failure here is unexpected
          case verifyLeiosVote committee eId ebHash (LeiosNonPersistentVote npVote) of
            Left verifyErr -> Left verifyErr
            Right _weight -> Right ()

-- | Create a private key for a given pool ID using the same test key derivation
makePrivateKey :: PoolId -> PrivateKeyLeios 'Vote
makePrivateKey pId = PrivateKeyLeios (testNetworkId, toSkForBLS pId)

-- | Test network ID for generating keys
testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

-- | Test election ID
testElectionId :: ElectionId
testElectionId = 42

-- | Test endorser block hash
testEndorserBlockHash :: EndorserBlockHash
testEndorserBlockHash = Hash.castHash $ Hash.hashWith id $ BSC.pack "test-endorser-block"

-- | Property test verifying CBOR serialization roundtrip for persistent votes
-- This test:
-- 1. Generates a random committee
-- 2. Creates persistent votes for all parties with persistent seats
-- 3. Serializes each vote to CBOR and deserializes it back
-- 4. Verifies the deserialized vote equals the original
prop_persistentVoteCBORRoundtrip :: Property
prop_persistentVoteCBORRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        -- Create all possible persistent votes
        persistentVotes =
          rights
            [createPersistentVote committee (makePrivateKey (poolId party)) eId ebHash | party <- parties osp]
        -- Test CBOR roundtrip for each vote
        roundtripResults = map testPersistentVoteCBORRoundtrip persistentVotes
        failures = lefts roundtripResults
     in counterexample
          ( "Number of persistent votes: "
              ++ show (length persistentVotes)
              ++ "\nRoundtrip failures: "
              ++ show (length failures)
              ++ "\nFailure details: "
              ++ show failures
          )
          $ null failures

-- | Test CBOR roundtrip for a single persistent vote
testPersistentVoteCBORRoundtrip :: PersistentVote -> Either String ()
testPersistentVoteCBORRoundtrip pv = do
  let encoded = BSL.fromStrict $ serialize' pv
  case decodeFull encoded of
    Left err -> Left $ "Failed to decode persistent vote: " ++ show err
    Right pv' ->
      if pv == pv'
        then Right ()
        else Left "Decoded persistent vote does not match original"

-- | Property test verifying CBOR serialization roundtrip for non-persistent votes
-- This test:
-- 1. Generates a random committee
-- 2. Creates non-persistent votes (only those that pass VRF threshold)
-- 3. Serializes each vote to CBOR and deserializes it back
-- 4. Verifies the deserialized vote equals the original
prop_nonPersistentVoteCBORRoundtrip :: Property
prop_nonPersistentVoteCBORRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        nonPersistentVotersData = voters $ nonPersistentVoters committee
        -- Create all possible non-persistent votes (only successful ones that pass VRF)
        nonPersistentVotes =
          rights
            [ createNonPersistentVote nonce committee (makePrivateKey pId) eId ebHash
            | pId <- Map.keys nonPersistentVotersData
            ]
        -- Test CBOR roundtrip for each vote
        roundtripResults = map testNonPersistentVoteCBORRoundtrip nonPersistentVotes
        failures = lefts roundtripResults
     in counterexample
          ( "Number of non-persistent votes: "
              ++ show (length nonPersistentVotes)
              ++ "\nRoundtrip failures: "
              ++ show (length failures)
              ++ "\nFailure details: "
              ++ show failures
          )
          $ null failures

-- | Test CBOR roundtrip for a single non-persistent vote
testNonPersistentVoteCBORRoundtrip :: NonPersistentVote -> Either String ()
testNonPersistentVoteCBORRoundtrip npv = do
  let encoded = BSL.fromStrict $ serialize' npv
  case decodeFull encoded of
    Left err -> Left $ "Failed to decode non-persistent vote: " ++ show err
    Right npv' ->
      if npv == npv'
        then Right ()
        else Left "Decoded non-persistent vote does not match original"

-- | Property test verifying CBOR serialization roundtrip for LeiosVote
-- This test combines both persistent and non-persistent votes
prop_leiosVoteCBORRoundtrip :: Property
prop_leiosVoteCBORRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        committee = wFA nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        -- Create all possible persistent votes
        persistentVotes =
          map LeiosPersistentVote $
            rights
              [createPersistentVote committee (makePrivateKey (poolId party)) eId ebHash | party <- parties osp]
        -- Create all possible non-persistent votes
        nonPersistentVotersData = voters $ nonPersistentVoters committee
        nonPersistentVotes =
          map LeiosNonPersistentVote $
            rights
              [ createNonPersistentVote nonce committee (makePrivateKey pId) eId ebHash
              | pId <- Map.keys nonPersistentVotersData
              ]
        -- Combine all votes
        allVotes = persistentVotes ++ nonPersistentVotes
        -- Test CBOR roundtrip for each vote
        roundtripResults = map testLeiosVoteCBORRoundtrip allVotes
        failures = lefts roundtripResults
     in counterexample
          ( "Number of persistent votes: "
              ++ show (length persistentVotes)
              ++ "\nNumber of non-persistent votes: "
              ++ show (length nonPersistentVotes)
              ++ "\nTotal votes: "
              ++ show (length allVotes)
              ++ "\nRoundtrip failures: "
              ++ show (length failures)
              ++ "\nFailure details: "
              ++ show failures
          )
          $ null failures

-- | Test CBOR roundtrip for a single LeiosVote
testLeiosVoteCBORRoundtrip :: LeiosVote -> Either String ()
testLeiosVoteCBORRoundtrip lv = do
  let encoded = BSL.fromStrict $ serialize' lv
  case decodeFull encoded of
    Left err -> Left $ "Failed to decode leios vote: " ++ show err
    Right lv' ->
      if lv == lv'
        then Right ()
        else Left "Decoded leios vote does not match original"
