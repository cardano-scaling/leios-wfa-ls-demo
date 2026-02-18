{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Leios.Vote where

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Crypto.Util (writeBinaryWord64)
import Cardano.Leios.Crypto (
  OutputVRF,
  Vote,
  checkVRFThreshold,
  coercePublicKeyLeios,
  verifyWithRoleLeios,
 )
import Cardano.Leios.Types
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  PersistentSeat (..),
  PersistentVoterIndex,
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

-- | Verify a `LeiosVote`'s validity against and `ElectionId` for a given `EndorserblockHash`, `PraosNonce`
-- and a certain `CommitteeSelection`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyLeiosVote ::
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  PraosNonce ->
  LeiosVote ->
  Either String Weight
verifyLeiosVote CommitteeSelection {persistentSeats, nonPersistentVoters} eId ebHash nonce vote = case vote of
  LeiosPersistentVote pv@PersistentVote {pvPersistentVoterId = pvId} -> case Map.lookup pvId persistentSeats of
    Nothing -> Left "verifyLeiosVote: persistent voter Id not found in committee"
    Just seat -> do
      verifyPersistentVote seat eId ebHash pv
      Right (weightPersistentSeat seat)
  LeiosNonPersistentVote npv@NonPersistentVote {npvPoolId = npvId} -> case Map.lookup npvId (voters nonPersistentVoters) of
    Nothing -> Left "verifyLeiosVote: non-persistent voter Id not found in committee"
    Just voter -> verifyNonPersistentVote voter eId ebHash nonce npv

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
        (writeBinaryWord64 nonce <> hashToBytes ebHash)
        vrfOutput
      checkVRFThreshold (stakeNonPersistentVoter voter) vrfOutput
