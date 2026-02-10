{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Leios.Vote where

import Cardano.Leios.Committee (PoolID)
import Cardano.Leios.Crypto (
  ElectionID,
  EndorserBlockHash,
  OutputVRF,
  Vote,
  Weight,
  checkVRFOutput,
  checkVRFThreshold,
  checkVoteSignature,
 )
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
  { pvElectionID :: ElectionID
  , pvPersistentVoterID :: PersistentVoterIndex
  , pvEndorserBlockHash :: EndorserBlockHash
  , pvVoteSignature :: Vote
  }

-- | A `NonPersistentVote` signals that a `NonPersistentVoter` agrees with an `EndorserBlock`.
data NonPersistentVote = NonPersistentVote
  { npvElectionID :: ElectionID
  , npvPoolID :: PoolID
  , npvEligibilitySignature :: OutputVRF
  , npvEndorserBlockHash :: EndorserBlockHash
  , npvVoteSignature :: Vote
  }

-- | Verify a `LeiosVote`'s validity against and `ElectionID` for a given `EndorserblockHash`
-- and a certain `CommitteeSelection`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyLeiosVote ::
  CommitteeSelection -> ElectionID -> EndorserBlockHash -> LeiosVote -> Either String Weight
verifyLeiosVote CommitteeSelection {persistentSeats, nonPersistentVoters} eID ebHash vote = case vote of
  LeiosPersistentVote pv@PersistentVote {pvPersistentVoterID = pvID} -> case Map.lookup pvID persistentSeats of
    Nothing -> Left "verifyLeiosVote: persistent voter ID not found in committee"
    Just seat -> do
      verifyPersistentVote seat eID ebHash pv
      Right (weightPersistentSeat seat)
  LeiosNonPersistentVote npv@NonPersistentVote {npvPoolID = npvID} -> case Map.lookup npvID (voters nonPersistentVoters) of
    Nothing -> Left "verifyLeiosVote: non-persistent voter ID not found in committee"
    Just voter -> verifyNonPersistentVote voter eID ebHash npv

-- | Verify a `PersistentVote`'s validity against and `ElectionID` for a given `EndorserblockHash`
-- and a certain `PersistentSeat`.
verifyPersistentVote ::
  PersistentSeat -> ElectionID -> EndorserBlockHash -> PersistentVote -> Either String ()
verifyPersistentVote seat eID ebHash vote
  | eID /= pvElectionID vote =
      Left "verifyPersistentVote: election ID in vote does not match input election ID"
  | ebHash /= pvEndorserBlockHash vote =
      Left "verifyPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = checkVoteSignature (publicVoteKeyPersistent seat) ebHash (pvVoteSignature vote)

-- | Verify a `NonPersistentVote`'s validity against and `ElectionID` for a given `EndorserblockHash`
-- and a certain `NonPersistentVoter`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyNonPersistentVote ::
  NonPersistentVoter -> ElectionID -> EndorserBlockHash -> NonPersistentVote -> Either String Weight
verifyNonPersistentVote voter eID ebHash vote
  | eID /= npvElectionID vote =
      Left "verifyNonPersistentVote: election ID in vote does not match input election ID"
  | ebHash /= npvEndorserBlockHash vote =
      Left "verifyNonPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = do
      let vk = publicVoteKeyNonPersistent voter
          sig = npvVoteSignature vote
          vrfOutput = npvEligibilitySignature vote
          stake = stakeNonPersistentVoter voter
      checkVoteSignature vk ebHash sig
      -- note that we currently do not add the praos randomness here
      -- we should use something like
      --
      -- checkVRFOutput vk (PraosNonceAsBS <> (writeBinaryWord64 eID)) vrfOutput
      --
      checkVRFOutput vk eID vrfOutput
      checkVRFThreshold stake vrfOutput
