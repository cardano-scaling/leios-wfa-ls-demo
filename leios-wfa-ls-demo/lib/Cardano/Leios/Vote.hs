{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Leios.Vote where

import Cardano.Api (PraosNonce, serialiseToRawBytes)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN))
import Cardano.Crypto.Util (writeBinaryWord64)
import Cardano.Leios.Crypto (
  KeyRoleLeios (..),
  OutputVRF,
  PrivateKeyLeios (..),
  PublicKeyLeios (PublicKeyLeios),
  Vote,
  checkVRFThreshold,
  coercePrivateKeyLeios,
  coercePublicKeyLeios,
  signWithRoleLeios,
  verifyWithRoleLeios,
 )
import Cardano.Leios.Types
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  PersistentSeat (..),
  PersistentVoterIndex,
  findNonPersistentVoterByPublicKey,
  findPersistentSeatByPublicKey,
 )
import qualified Data.Map as Map

-- | A `LeiosVote` is either from a persistent voter or a lottery based non-persistent voter.
data LeiosVote = LeiosPersistentVote PersistentVote | LeiosNonPersistentVote NonPersistentVote

-- | A `PersistentVote` signals that a `PersistentVoter` agrees with an `EndorserBlock`.
data PersistentVote = PersistentVote
  { pvElectionId :: ElectionId
  , pvPersistentVoterId :: PersistentVoterIndex
  , pvEndorserBlockHash :: EndorserBlockHash
  , pvVoteSignature :: Vote
  }

-- | A `NonPersistentVote` signals that a `NonPersistentVoter` agrees with an `EndorserBlock`.
data NonPersistentVote = NonPersistentVote
  { npvElectionId :: ElectionId
  , npvPoolId :: PoolId
  , npvEligibilitySignature :: OutputVRF
  , npvEndorserBlockHash :: EndorserBlockHash
  , npvVoteSignature :: Vote
  }

-- | Verify a `LeiosVote`'s validity against and `ElectionId` for a given `EndorserblockHash`
-- and a certain `CommitteeSelection`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyLeiosVote ::
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  LeiosVote ->
  Either String Weight
verifyLeiosVote CommitteeSelection {persistentSeats, nonPersistentVoters, praosNonce} eId ebHash vote = case vote of
  LeiosPersistentVote pv@PersistentVote {pvPersistentVoterId = pvId} -> case Map.lookup pvId persistentSeats of
    Nothing -> Left "verifyLeiosVote: persistent voter Id not found in committee"
    Just seat -> do
      verifyPersistentVote seat eId ebHash pv
      Right (weightPersistentSeat seat)
  LeiosNonPersistentVote npv@NonPersistentVote {npvPoolId = npvId} -> case Map.lookup npvId (voters nonPersistentVoters) of
    Nothing -> Left "verifyLeiosVote: non-persistent voter Id not found in committee"
    Just voter -> verifyNonPersistentVote voter eId ebHash praosNonce npv

-- | Verify a `PersistentVote`'s validity against and `ElectionId` for a given `EndorserblockHash`
-- and a certain `PersistentSeat`.
verifyPersistentVote ::
  PersistentSeat -> ElectionId -> EndorserBlockHash -> PersistentVote -> Either String ()
verifyPersistentVote seat eId ebHash vote
  | eId /= pvElectionId vote =
      Left "verifyPersistentVote: election Id in vote does not match input election Id"
  | ebHash /= pvEndorserBlockHash vote =
      Left "verifyPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = verifyWithRoleLeios (publicVoteKeyPersistent seat) ebHash (pvVoteSignature vote)

-- | Verify a `NonPersistentVote`'s validity against and `ElectionId` for a given `EndorserblockHash`
-- and a certain `NonPersistentVoter`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyNonPersistentVote ::
  NonPersistentVoter ->
  ElectionId ->
  EndorserBlockHash ->
  PraosNonce ->
  NonPersistentVote ->
  Either String Weight
verifyNonPersistentVote voter eId ebHash nonce vote
  | eId /= npvElectionId vote =
      Left "verifyNonPersistentVote: election Id in vote does not match input election Id"
  | ebHash /= npvEndorserBlockHash vote =
      Left "verifyNonPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = do
      let pkVote = publicVoteKeyNonPersistent voter
          vrfOutput = npvEligibilitySignature vote
      verifyWithRoleLeios pkVote ebHash (npvVoteSignature vote)
      verifyWithRoleLeios
        (coercePublicKeyLeios pkVote)
        (serialiseToRawBytes nonce <> writeBinaryWord64 eId)
        vrfOutput
      checkVRFThreshold (stakeNonPersistentVoter voter) vrfOutput

createPersistentVote ::
  CommitteeSelection ->
  PrivateKeyLeios 'Vote ->
  ElectionId ->
  EndorserBlockHash ->
  Either String PersistentVote
createPersistentVote committee privKey@(PrivateKeyLeios (nId, sk)) eId ebHash = case findPersistentSeatByPublicKey pk (persistentSeats committee) of
  Nothing -> Left "createPersistentVote: no persistent seat found for this key"
  Just (pIx, _) ->
    Right $
      PersistentVote
        { pvElectionId = eId
        , pvPersistentVoterId = pIx
        , pvEndorserBlockHash = ebHash
        , pvVoteSignature = signWithRoleLeios ebHash privKey
        }
  where
    pk = PublicKeyLeios (nId, deriveVerKeyDSIGN sk) :: PublicKeyLeios 'Vote

createNonPersistentVote ::
  PraosNonce ->
  CommitteeSelection ->
  PrivateKeyLeios 'Vote ->
  ElectionId ->
  EndorserBlockHash ->
  Either String NonPersistentVote
createNonPersistentVote nonce commitee privKey@(PrivateKeyLeios (nId, sk)) eId ebHash = case findNonPersistentVoterByPublicKey pk vtrs of
  Nothing -> Left "createNonPersistentVote: no non-persistent voter found for this key"
  Just (pId, npv) ->
    let npvRelativeStake = stakeNonPersistentVoter npv
        outputVRF =
          signWithRoleLeios
            (serialiseToRawBytes nonce <> writeBinaryWord64 eId)
            (coercePrivateKeyLeios privKey)
     in case checkVRFThreshold npvRelativeStake outputVRF of
          Left err -> Left err
          Right _ ->
            Right $
              NonPersistentVote
                { npvElectionId = eId
                , npvPoolId = pId
                , npvEligibilitySignature = outputVRF
                , npvEndorserBlockHash = ebHash
                , npvVoteSignature = signWithRoleLeios ebHash privKey
                }
  where
    vtrs = voters . nonPersistentVoters $ commitee
    pk = PublicKeyLeios (nId, deriveVerKeyDSIGN sk) :: PublicKeyLeios 'Vote
