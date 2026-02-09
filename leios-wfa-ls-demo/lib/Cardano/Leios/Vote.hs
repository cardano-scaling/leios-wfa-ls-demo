{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Leios.Vote where

import Cardano.Leios.Committee (PoolID)
import Cardano.Leios.Crypto (
  ElectionID,
  EndorserBlockHash,
  OutputVRF,
  Vote,
  Weight,
  checkEligibilitySignature,
  checkVoteSignature,
 )
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  PersistentSeat (..),
  PersistentVoterId,
 )
import qualified Data.Map as Map

data LeiosVote = LeiosPersistentVote PersistentVote | LeiosNonPersistentVote NonPersistentVote

data PersistentVote = PersistentVote
  { pvElectionID :: ElectionID
  , pvPersistentVoterID :: PersistentVoterId
  , pvEndorserBlockHash :: EndorserBlockHash
  , pvVoteSignature :: Vote
  }

data NonPersistentVote = NonPersistentVote
  { npvElectionID :: ElectionID
  , npvPoolID :: PoolID
  , npvEligibilitySignature :: OutputVRF
  , npvEndorserBlockHash :: EndorserBlockHash
  , npvVoteSignature :: Vote
  }

verifyLeiosVote ::
  ElectionID -> EndorserBlockHash -> CommitteeSelection -> LeiosVote -> Either String Weight
verifyLeiosVote eID ebHash CommitteeSelection {persistentSeats, nonPersistentVoters} vote = case vote of
  LeiosPersistentVote pv@PersistentVote {pvPersistentVoterID = pvID} -> case Map.lookup pvID persistentSeats of
    Nothing -> Left "verifyLeiosVote: persistent voter ID not found in committee"
    Just seat -> do
      verifyPersistentVote eID ebHash seat pv
      Right (weightPersistentSeat seat)
  LeiosNonPersistentVote npv@NonPersistentVote {npvPoolID = npvID} -> case Map.lookup npvID (voters nonPersistentVoters) of
    Nothing -> Left "verifyLeiosVote: non-persistent voter ID not found in committee"
    Just voter -> verifyNonPersistentVote eID ebHash voter npv

verifyPersistentVote ::
  ElectionID -> EndorserBlockHash -> PersistentSeat -> PersistentVote -> Either String ()
verifyPersistentVote eID ebHash seat vote
  | eID /= pvElectionID vote =
      Left "verifyPersistentVote: election ID in vote does not match input election ID"
  | ebHash /= pvEndorserBlockHash vote =
      Left "verifyPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = checkVoteSignature (publicVoteKeyPersistent seat) ebHash (pvVoteSignature vote)

verifyNonPersistentVote ::
  ElectionID -> EndorserBlockHash -> NonPersistentVoter -> NonPersistentVote -> Either String Weight
verifyNonPersistentVote eID ebHash voter vote
  | eID /= npvElectionID vote =
      Left "verifyNonPersistentVote: election ID in vote does not match input election ID"
  | ebHash /= npvEndorserBlockHash vote =
      Left "verifyNonPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = do
      let vk = publicVoteKeyNonPersistent voter
          sig = npvVoteSignature vote
      checkVoteSignature vk ebHash sig
      checkEligibilitySignature vk (stakeNonPersistentVoter voter) eID sig
