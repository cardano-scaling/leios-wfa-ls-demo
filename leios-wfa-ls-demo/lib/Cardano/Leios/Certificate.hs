{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.Certificate (
  -- * Certificate type
  Certificate (..),

  -- * Certificate creation
  createCertificate,

  -- * Certificate verification
  verifyCertificate,
) where

import Cardano.Api (serialiseToRawBytes)
import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN (DSIGNAggregatable (..), DSIGNAlgorithm (..))
import Cardano.Crypto.DSIGN.BLS12381 (BLS12381MinSigDSIGN)
import Cardano.Crypto.Util (writeBinaryWord64)
import Cardano.Leios.BitMapPV (BitMapPV, bitmapFromIndexes, getAllFlippedIndexes)
import Cardano.Leios.Crypto (
  HasBLSContext (..),
  KeyRoleLeios (..),
  OutputVRF,
  PublicKeyLeios (..),
  SignatureLeios (..),
  Vote,
 )
import Cardano.Leios.Types (ElectionId, EndorserBlockHash, PoolId, Weight)
import Cardano.Leios.Vote (
  LeiosVote (..),
  NonPersistentVote (..),
  PersistentVote (..),
  checkNonPersistentVoteEligibility,
  npvEligibilitySignature,
  npvPoolId,
  npvVoteSignature,
  pvPersistentVoterId,
  pvVoteSignature,
 )
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  PersistentSeat (..),
  PersistentVoterIndex,
  publicVoteKeyNonPersistent,
  publicVoteKeyPersistent,
  weightPersistentSeat,
 )
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import qualified Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))

-- | A certificate aggregates votes from both persistent and non-persistent voters
-- to endorse an endorser block.
--
-- leios_certificate =
--   [ election_id              : election_id
--   , endorser_block_hash      : hash32
--   , persistent_voters        : [* persistent_voter_id]
--   , nonpersistent_voters     : {* pool_id => leios_bls_signature}
--   , aggregated_vote_sig      : leios_bls_signature
--   ]
data Certificate = Certificate
  { certElectionId :: ElectionId
  , certEndorserBlockHash :: EndorserBlockHash
  , pvVoters :: BitMapPV
  , -- For non-persistent voters, we only store the eligibility signature (OutputVRF)
    -- Individual vote signatures are not stored, only aggregated in aggrVote
    npvVoters :: Map PoolId OutputVRF
  , -- Aggregate of all NPV OutputVRF values (eligibility signatures)
    -- This enables batch verification optimization
    npvAggr :: Maybe OutputVRF
  , -- Aggregate of ALL vote signatures (both PV and NPV)
    aggrVote :: Vote
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- BLS Signature Aggregation Helpers
--------------------------------------------------------------------------------

-- | Aggregate a list of Vote signatures into a single aggregated signature
aggregateVotes :: [Vote] -> Either String Vote
aggregateVotes [] = Left "aggregateVotes: cannot aggregate empty list of votes"
aggregateVotes votes =
  let unwrappedSigs = map (\(SignatureLeios sig) -> sig) votes
   in case aggregateSigsDSIGN @BLS12381MinSigDSIGN unwrappedSigs of
        Left err -> Left $ "aggregateVotes: " ++ err
        Right aggrSig -> Right (SignatureLeios aggrSig)

-- | Aggregate a list of OutputVRF signatures into a single aggregated signature
-- OutputVRF and Vote are both BLS signatures, so we can use the same aggregation
aggregateOutputVRFs :: [OutputVRF] -> Either String OutputVRF
aggregateOutputVRFs [] = Left "aggregateOutputVRFs: cannot aggregate empty list"
aggregateOutputVRFs vrfOutputs =
  let unwrappedSigs = map (\(SignatureLeios sig) -> sig) vrfOutputs
   in case aggregateSigsDSIGN @BLS12381MinSigDSIGN unwrappedSigs of
        Left err -> Left $ "aggregateOutputVRFs: " ++ err
        Right aggrSig -> Right (SignatureLeios aggrSig)

-- | Create a certificate from a list of Leios votes
-- All votes must be for the specified election ID and endorser block hash
-- This function assumes the votes are valid
createCertificate ::
  -- | Election ID this certificate is for
  ElectionId ->
  -- | Endorser block hash this certificate endorses
  EndorserBlockHash ->
  -- | Committee selection (used to determine max persistent voter index)
  CommitteeSelection ->
  -- | List of votes to aggregate
  [LeiosVote] ->
  Either String Certificate
createCertificate eId ebHash committee votes
  | null votes = Left "createCertificate: cannot create certificate from empty vote list"
  | otherwise = do
      -- Verify all votes are for the expected election ID and endorser block hash
      let checkVote v = case v of
            LeiosPersistentVote pv ->
              pvElectionId pv == eId && pvEndorserBlockHash pv == ebHash
            LeiosNonPersistentVote npv ->
              npvElectionId npv == eId && npvEndorserBlockHash npv == ebHash

      if not (all checkVote votes)
        then Left "createCertificate: not all votes match the expected election ID and endorser block hash"
        else do
          -- Separate persistent and non-persistent votes
          let (pvs, npvs) = partitionVotes votes

          -- Get max persistent voter index from committee
          let maxPvIndex = case Map.lookupMax (persistentSeats committee) of
                Nothing -> 0 -- No persistent voters
                Just (maxIdx, _) -> maxIdx

          -- Build bitmap from persistent voter indices
          let pvIndices = map pvPersistentVoterId pvs
              bitmap = bitmapFromIndexes pvIndices maxPvIndex

          -- Build map and collect eligibility signatures in one pass (single iteration over npvs)
          let (npvMap, npvEligSigs) =
                foldr
                  ( \npv (accMap, accSigs) ->
                      let poolId = npvPoolId npv
                          eligSig = npvEligibilitySignature npv
                       in (Map.insert poolId eligSig accMap, eligSig : accSigs)
                  )
                  (Map.empty, [])
                  npvs

          -- Aggregate non-persistent OutputVRF values (eligibility signatures) for batch verification
          npvAggregate <-
            if null npvEligSigs
              then Right Nothing
              else case aggregateOutputVRFs npvEligSigs of
                Left err -> Left err
                Right aggr -> Right (Just aggr)

          -- Aggregate all vote signatures (both PV and NPV)
          let allSigs = map pvVoteSignature pvs ++ map npvVoteSignature npvs
          aggrSig <- aggregateVotes allSigs

          Right $
            Certificate
              { certElectionId = eId
              , certEndorserBlockHash = ebHash
              , pvVoters = bitmap
              , npvVoters = npvMap
              , npvAggr = npvAggregate
              , aggrVote = aggrSig
              }
  where
    partitionVotes :: [LeiosVote] -> ([PersistentVote], [NonPersistentVote])
    partitionVotes = foldr go ([], [])
      where
        go (LeiosPersistentVote pv) (pvs, npvs) = (pv : pvs, npvs)
        go (LeiosNonPersistentVote npv) (pvs, npvs) = (pvs, npv : npvs)

-- | Verify a certificate against a committee selection
-- Returns the total weight of votes if valid
verifyCertificate ::
  -- | Election ID to verify against
  ElectionId ->
  -- | Endorser block hash to verify against
  EndorserBlockHash ->
  -- | Committee selection for this election
  CommitteeSelection ->
  -- | Certificate to verify
  Certificate ->
  Either String Weight
verifyCertificate eId ebHash committee Certificate {certElectionId, certEndorserBlockHash, pvVoters, npvVoters, npvAggr, aggrVote}
  -- Step 1: Verify the certificate is for the expected election and endorser block (fail fast)
  | certElectionId /= eId =
      Left "verifyCertificate: certificate election ID does not match expected election ID"
  | certEndorserBlockHash /= ebHash =
      Left "verifyCertificate: certificate endorser block hash does not match expected hash"
  | otherwise = do
      -- Step 2: Lookup public keys of PV voters from bitmap
      let pvIndices = getAllFlippedIndexes pvVoters
      pvSeats <- mapM lookupPersistentSeat pvIndices
      let pvPubKeys = map (unwrapPublicKey . publicVoteKeyPersistent) pvSeats

      -- Step 3: Lookup NPV voters and extract their public keys and eligibility signatures
      let (npvPoolIds, npvEligSigs) = unzip (Map.toList npvVoters)
      npvVotersList <- mapM lookupNonPersistentVoterByPoolId npvPoolIds
      let npvPubKeys = map (unwrapPublicKey . publicVoteKeyNonPersistent) npvVotersList
          -- Get network ID from committee (same for all voters)
          nId = networkId committee

      -- Step 3.5: Aggregate NPV public keys (if any) - will be reused in Steps 4 and 5
      npvAggrPubKey <- case npvPubKeys of
        [] -> Right Nothing
        -- This is not really nice, as this prevents us from swapping the underlying signature scheme
        -- easily. TODO: properly abstract this away.
        _ -> case uncheckedAggregateVerKeysDSIGN @BLS12381MinSigDSIGN npvPubKeys of
          Left err -> Left $ "verifyCertificate: failed to aggregate NPV public keys: " ++ err
          Right key -> Right (Just key)

      -- Step 4: Verify NPV aggregate eligibility signature (fail fast)
      -- Only verify if there are NPV voters
      case npvAggrPubKey of
        Nothing -> Right () -- No NPV voters, skip verification
        Just aggrKey -> do
          -- Check that NPV aggregate signature exists
          npvAggrSig <- case npvAggr of
            Nothing -> Left "verifyCertificate: certificate has NPV voters but missing NPV aggregate signature"
            Just sig -> Right sig

          -- Verify NPV aggregate eligibility signature against Nonce || eId
          -- Note: VRF outputs use the 'VRF role, not 'Vote role
          let nonce = praosNonce committee
              (SignatureLeios npvAggrSigRaw) = npvAggrSig

          case verifyDSIGN
            (blsCtx (Proxy @'VRF) nId)
            aggrKey
            (serialiseToRawBytes nonce <> writeBinaryWord64 eId)
            npvAggrSigRaw of
            Left err -> Left $ "verifyCertificate: NPV aggregate eligibility signature verification failed: " ++ err
            Right () -> Right ()

      -- Step 5: Combine NPV aggregate key with all PV keys and verify aggregate vote signature
      let allPubKeys = case npvAggrPubKey of
            Nothing -> pvPubKeys
            Just aggrKey -> aggrKey : pvPubKeys

      case allPubKeys of
        [] -> Left "verifyCertificate: no voters in certificate"
        _ -> do
          finalAggrPubKey <- case uncheckedAggregateVerKeysDSIGN @BLS12381MinSigDSIGN allPubKeys of
            Left err -> Left $ "verifyCertificate: failed to aggregate final public keys: " ++ err
            Right key -> Right key

          let (SignatureLeios aggrSig) = aggrVote
          case verifyDSIGN (blsCtx (Proxy @'Vote) nId) finalAggrPubKey ebHash aggrSig of
            Left err -> Left $ "verifyCertificate: aggregated vote signature verification failed: " ++ err
            Right () -> Right ()

      -- Step 6: Verify eligibility for each NPV (VRF output → k seats, no signature checks)
      npvWeights <-
        Control.Monad.zipWithM (checkNonPersistentVoteEligibility committee) npvVotersList npvEligSigs

      -- Step 7: Calculate and return total weight
      let pvWeights = map weightPersistentSeat pvSeats
      Right (sum pvWeights + sum npvWeights)
  where
    lookupPersistentSeat :: PersistentVoterIndex -> Either String PersistentSeat
    lookupPersistentSeat pvIdx =
      case Map.lookup pvIdx (persistentSeats committee) of
        Nothing -> Left $ "verifyCertificate: persistent voter index " ++ show pvIdx ++ " not in committee"
        Just seat -> Right seat

    lookupNonPersistentVoterByPoolId :: PoolId -> Either String NonPersistentVoter
    lookupNonPersistentVoterByPoolId poolId =
      case Map.lookup poolId (voters $ nonPersistentVoters committee) of
        Nothing -> Left "verifyCertificate: non-persistent voter pool ID not in committee"
        Just voter -> Right voter

    -- Helper to unwrap PublicKeyLeios to get the raw BLS verification key
    unwrapPublicKey :: PublicKeyLeios 'Vote -> VerKeyDSIGN BLS12381MinSigDSIGN
    unwrapPublicKey (PublicKeyLeios (_, vk)) = vk

--------------------------------------------------------------------------------
-- CBOR Serialization
--------------------------------------------------------------------------------

-- Helper to encode/decode the NPV map (only OutputVRF, not vote signatures)
encodeNpvMap :: Map PoolId OutputVRF -> E.Encoding
encodeNpvMap m =
  E.encodeMapLen (fromIntegral $ Map.size m)
    <> mconcat
      [ toCBOR poolId <> toCBOR eligSig
      | (poolId, eligSig) <- Map.toList m
      ]

decodeNpvMap :: D.Decoder s (Map PoolId OutputVRF)
decodeNpvMap = do
  len <- D.decodeMapLen
  Map.fromList <$> sequence [decodeEntry | _ <- [1 .. len]]
  where
    decodeEntry = do
      !poolId <- fromCBOR
      !eligSig <- fromCBOR
      return (poolId, eligSig)

-- | ToCBOR instance for Certificate
-- Encodes as: [election_id, endorser_block_hash, pv_voters, npv_voters, npv_aggr, aggr_vote]
instance ToCBOR Certificate where
  toCBOR Certificate {certElectionId, certEndorserBlockHash, pvVoters, npvVoters, npvAggr, aggrVote} =
    E.encodeListLen 6
      <> toCBOR certElectionId
      <> toCBOR certEndorserBlockHash
      <> toCBOR pvVoters
      <> encodeNpvMap npvVoters
      <> toCBOR npvAggr
      <> toCBOR aggrVote

-- | FromCBOR instance for Certificate
instance FromCBOR Certificate where
  fromCBOR = do
    D.decodeListLenOf 6
    !eId <- fromCBOR
    !ebHash <- fromCBOR
    !pvVoters <- fromCBOR
    !npvVoters <- decodeNpvMap
    !npvAggr <- fromCBOR
    !aggrVote <- fromCBOR
    return $
      Certificate
        { certElectionId = eId
        , certEndorserBlockHash = ebHash
        , pvVoters = pvVoters
        , npvVoters = npvVoters
        , npvAggr = npvAggr
        , aggrVote = aggrVote
        }
