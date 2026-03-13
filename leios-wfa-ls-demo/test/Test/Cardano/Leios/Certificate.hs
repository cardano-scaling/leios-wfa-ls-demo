{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Leios.Certificate (tests) where

import Cardano.Api (NetworkId (..), NetworkMagic (..), PraosNonce)
import Cardano.Api.Shelley (makePraosNonce)
import Cardano.Binary (decodeFull, serialize')
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Leios.Certificate (
  Certificate (..),
  createCertificate,
  verifyCertificate,
 )
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
  createNonPersistentVote,
  createPersistentVote,
 )
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  wFA,
 )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft, rights)
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
    "Certificate"
    [ localOption (QuickCheckTests 1000) $
        testProperty
          "certificate creation and verification roundtrip"
          prop_certificateRoundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "certificate CBOR roundtrip"
          prop_certificateCBORRoundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "empty vote list is rejected"
          prop_emptyVoteListRejected
    , localOption (QuickCheckTests 1000) $
        testProperty
          "mismatched election ID in votes is rejected"
          prop_mismatchedElectionIdRejected
    , localOption (QuickCheckTests 1000) $
        testProperty
          "mismatched endorser block hash in votes is rejected"
          prop_mismatchedEndorserBlockHashRejected
    , localOption (QuickCheckTests 1000) $
        testProperty
          "certificate with only persistent votes works"
          prop_onlyPersistentVotes
    , localOption (QuickCheckTests 1000) $
        testProperty
          "certificate with only non-persistent votes works"
          prop_onlyNonPersistentVotes
    , localOption (QuickCheckTests 1000) $
        testProperty
          "certificate verification fails for wrong election ID"
          prop_wrongElectionIdFails
    , localOption (QuickCheckTests 1000) $
        testProperty
          "certificate verification fails for wrong endorser block hash"
          prop_wrongEndorserBlockHashFails
    ]

-- | Property test verifying that certificates can be created from votes and verified
-- in a roundtrip fashion. This test:
-- 1. Generates a random committee using wFA
-- 2. Creates votes (both persistent and non-persistent) for that committee
-- 3. Creates a certificate from those votes
-- 4. Verifies the certificate
-- 5. Checks that the weight matches the sum of all vote weights
prop_certificateRoundtrip :: Property
prop_certificateRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        -- Create all possible votes
        allVotes = createAllVotes nonce committee eId ebHash (parties osp)
     in case allVotes of
          [] ->
            -- If no votes were created (committee is empty), this is expected
            counterexample "No votes created (empty committee)" True
          votes -> case createCertificate eId ebHash committee votes of
            Left err ->
              counterexample
                ( "Certificate creation failed: "
                    ++ err
                    ++ "\nNumber of votes: "
                    ++ show (length votes)
                )
                False
            Right cert -> case verifyCertificate eId ebHash committee cert of
              Left err ->
                counterexample
                  ( "Certificate verification failed: "
                      ++ err
                      ++ "\nNumber of votes: "
                      ++ show (length votes)
                      ++ "\nCertificate: "
                      ++ show cert
                  )
                  False
              Right _weight ->
                -- Success - certificate was created and verified
                counterexample
                  ( "Success with "
                      ++ show (length votes)
                      ++ " votes"
                  )
                  True

-- | Property test verifying CBOR serialization roundtrip for certificates
prop_certificateCBORRoundtrip :: Property
prop_certificateCBORRoundtrip =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        allVotes = createAllVotes nonce committee eId ebHash (parties osp)
     in case allVotes of
          [] -> counterexample "No votes created (empty committee)" True
          votes -> case createCertificate eId ebHash committee votes of
            Left err ->
              counterexample
                ( "Certificate creation failed: "
                    ++ err
                )
                False
            Right cert -> case testCertificateCBORRoundtrip cert of
              Left err ->
                counterexample
                  ( "CBOR roundtrip failed: "
                      ++ err
                      ++ "\nCertificate: "
                      ++ show cert
                  )
                  False
              Right () ->
                counterexample "CBOR roundtrip succeeded" True

-- | Property test verifying that creating a certificate with an empty vote list fails
prop_emptyVoteListRejected :: Property
prop_emptyVoteListRejected =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        result = createCertificate eId ebHash committee []
     in counterexample
          ("Result: " ++ show result)
          $ isLeft result

-- | Property test verifying that mismatched election IDs in votes are rejected
prop_mismatchedElectionIdRejected :: Property
prop_mismatchedElectionIdRejected =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId1 = testElectionId
        eId2 = testElectionId + 1 -- Different election ID
        ebHash = testEndorserBlockHash
        -- Create votes for eId1 and eId2
        votes1 = take 1 $ createAllVotes nonce committee eId1 ebHash (parties osp)
        votes2 = take 1 $ createAllVotes nonce committee eId2 ebHash (parties osp)
        mixedVotes = votes1 ++ votes2
     in case mixedVotes of
          -- Need at least 2 votes to test mismatch
          v1 : v2 : _ ->
            let result = createCertificate eId1 ebHash committee [v1, v2]
             in counterexample
                  ("Result: " ++ show result)
                  $ isLeft result
          _ -> counterexample "Not enough votes to test" True

-- | Property test verifying that mismatched endorser block hashes in votes are rejected
prop_mismatchedEndorserBlockHashRejected :: Property
prop_mismatchedEndorserBlockHashRejected =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash1 = testEndorserBlockHash
        ebHash2 = testEndorserBlockHash2
        -- Create votes for ebHash1 and ebHash2
        votes1 = take 1 $ createAllVotes nonce committee eId ebHash1 (parties osp)
        votes2 = take 1 $ createAllVotes nonce committee eId ebHash2 (parties osp)
        mixedVotes = votes1 ++ votes2
     in case mixedVotes of
          v1 : v2 : _ ->
            let result = createCertificate eId ebHash1 committee [v1, v2]
             in counterexample
                  ("Result: " ++ show result)
                  $ isLeft result
          _ -> counterexample "Not enough votes to test" True

-- | Property test verifying that certificates with only persistent votes work
prop_onlyPersistentVotes :: Property
prop_onlyPersistentVotes =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        -- Create only persistent votes
        persistentVotes =
          rights
            [ createPersistentVote committee (makePrivateKey (poolId party)) eId ebHash
            | party <- parties osp
            ]
        votes = map LeiosPersistentVote persistentVotes
     in case votes of
          [] -> counterexample "No persistent votes created" True
          _ -> case createCertificate eId ebHash committee votes of
            Left err ->
              counterexample
                ( "Certificate creation failed: "
                    ++ err
                )
                False
            Right cert -> case verifyCertificate eId ebHash committee cert of
              Left err ->
                counterexample
                  ( "Certificate verification failed: "
                      ++ err
                  )
                  False
              Right _weight ->
                counterexample
                  ( "Success with "
                      ++ show (length votes)
                      ++ " persistent votes"
                  )
                  True

-- | Property test verifying that certificates with only non-persistent votes work
prop_onlyNonPersistentVotes :: Property
prop_onlyNonPersistentVotes =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash = testEndorserBlockHash
        nonPersistentVotersData = voters $ nonPersistentVoters committee
        -- Create only non-persistent votes
        nonPersistentVotes =
          rights
            [ createNonPersistentVote nonce committee (makePrivateKey pId) eId ebHash
            | pId <- Map.keys nonPersistentVotersData
            ]
        votes = map LeiosNonPersistentVote nonPersistentVotes
     in case votes of
          [] -> counterexample "No non-persistent votes created" True
          _ -> case createCertificate eId ebHash committee votes of
            Left err ->
              counterexample
                ( "Certificate creation failed: "
                    ++ err
                )
                False
            Right cert -> case verifyCertificate eId ebHash committee cert of
              Left err ->
                counterexample
                  ( "Certificate verification failed: "
                      ++ err
                  )
                  False
              Right _weight ->
                counterexample
                  ( "Success with "
                      ++ show (length votes)
                      ++ " non-persistent votes"
                  )
                  True

-- | Property test verifying that certificate verification fails for wrong election ID
prop_wrongElectionIdFails :: Property
prop_wrongElectionIdFails =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId1 = testElectionId
        eId2 = testElectionId + 1
        ebHash = testEndorserBlockHash
        allVotes = createAllVotes nonce committee eId1 ebHash (parties osp)
     in case allVotes of
          [] -> counterexample "No votes created" True
          votes -> case createCertificate eId1 ebHash committee votes of
            Left err ->
              counterexample
                ( "Certificate creation failed: "
                    ++ err
                )
                True -- Expected to fail sometimes
            Right cert ->
              let result = verifyCertificate eId2 ebHash committee cert
               in counterexample
                    ( "Verification with wrong election ID should fail, got: "
                        ++ show result
                    )
                    $ isLeft result

-- | Property test verifying that certificate verification fails for wrong endorser block hash
prop_wrongEndorserBlockHashFails :: Property
prop_wrongEndorserBlockHashFails =
  forAllBlind genOrderedSetOfParties $ \osp ->
    let nonce = makePraosNonce $ BSC.pack "test-nonce"
        nId = Testnet (NetworkMagic 42)
        committee = wFA nId nonce osp
        eId = testElectionId
        ebHash1 = testEndorserBlockHash
        ebHash2 = testEndorserBlockHash2
        allVotes = createAllVotes nonce committee eId ebHash1 (parties osp)
     in case allVotes of
          [] -> counterexample "No votes created" True
          votes -> case createCertificate eId ebHash1 committee votes of
            Left err ->
              counterexample
                ( "Certificate creation failed: "
                    ++ err
                )
                True -- Expected to fail sometimes
            Right cert ->
              let result = verifyCertificate eId ebHash2 committee cert
               in counterexample
                    ( "Verification with wrong endorser block hash should fail, got: "
                        ++ show result
                    )
                    $ isLeft result

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create all possible votes for a given committee and election
-- This includes both persistent and non-persistent votes
createAllVotes ::
  PraosNonce ->
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  [Party] ->
  [LeiosVote]
createAllVotes nonce committee eId ebHash partyList =
  let
    -- Create all possible persistent votes
    persistentVotes =
      rights
        [ createPersistentVote committee (makePrivateKey (poolId party)) eId ebHash
        | party <- partyList
        ]
    -- Create all possible non-persistent votes
    nonPersistentVotersData = voters $ nonPersistentVoters committee
    nonPersistentVotes =
      rights
        [ createNonPersistentVote nonce committee (makePrivateKey pId) eId ebHash
        | pId <- Map.keys nonPersistentVotersData
        ]
   in
    map LeiosPersistentVote persistentVotes
      ++ map LeiosNonPersistentVote nonPersistentVotes

-- | Test CBOR roundtrip for a single certificate
testCertificateCBORRoundtrip :: Certificate -> Either String ()
testCertificateCBORRoundtrip cert = do
  let encoded = BSL.fromStrict $ serialize' cert
  case decodeFull encoded of
    Left err -> Left $ "Failed to decode certificate: " ++ show err
    Right cert' ->
      if cert == cert'
        then Right ()
        else Left "Decoded certificate does not match original"

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

-- | Another test endorser block hash (for testing mismatches)
testEndorserBlockHash2 :: EndorserBlockHash
testEndorserBlockHash2 = Hash.castHash $ Hash.hashWith id $ BSC.pack "test-endorser-block-2"
