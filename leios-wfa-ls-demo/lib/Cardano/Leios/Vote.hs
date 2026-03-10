{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.Vote where

import Cardano.Api (PraosNonce, serialiseToRawBytes)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN))
import Cardano.Crypto.Util (writeBinaryWord64)
import Cardano.Leios.Committee (NonPersistentSeats)
import Cardano.Leios.Crypto (
  KeyRoleLeios (..),
  OutputVRF,
  PrivateKeyLeios (..),
  PublicKeyLeios (PublicKeyLeios),
  Vote,
  coercePrivateKeyLeios,
  coercePublicKeyLeios,
  signWithRoleLeios,
  verifyWithRoleLeios,
 )
import Cardano.Leios.LocalSortition (checkLeaderValueLeios)
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
import Data.Ratio ((%))

-- | A `LeiosVote` is either from a persistent voter or a lottery-based non-persistent voter.
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

-- | Verify a `LeiosVote`'s validity against an `ElectionId` for a given `EndorserBlockHash`
-- and a certain `CommitteeSelection`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyLeiosVote ::
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  LeiosVote ->
  Either String Weight
verifyLeiosVote committee@CommitteeSelection {persistentSeats, nonPersistentVoters, praosNonce} eId ebHash vote = case vote of
  LeiosPersistentVote pv@PersistentVote {pvPersistentVoterId = pvId} -> case Map.lookup pvId persistentSeats of
    Nothing -> Left "verifyLeiosVote: persistent voter ID not found in committee"
    Just seat -> do
      verifyPersistentVote seat eId ebHash pv
      Right (weightPersistentSeat seat)
  LeiosNonPersistentVote npv@NonPersistentVote {npvPoolId = npvId} -> case Map.lookup npvId (voters nonPersistentVoters) of
    Nothing -> Left "verifyLeiosVote: non-persistent voter ID not found in committee"
    Just voter -> verifyNonPersistentVote committee voter eId ebHash praosNonce npv

-- | Verify a `PersistentVote`'s validity against an `ElectionId` for a given `EndorserBlockHash`
-- and a given `PersistentSeat`.
verifyPersistentVote ::
  PersistentSeat -> ElectionId -> EndorserBlockHash -> PersistentVote -> Either String ()
verifyPersistentVote seat eId ebHash vote
  | eId /= pvElectionId vote =
      Left "verifyPersistentVote: election ID in vote does not match input election ID"
  | ebHash /= pvEndorserBlockHash vote =
      Left "verifyPersistentVote: EB hash in vote does not match input EB hash"
  | otherwise = verifyWithRoleLeios (publicVoteKeyPersistent seat) ebHash (pvVoteSignature vote)

-- | Verify a `NonPersistentVote`'s validity against an `ElectionId` for a given `EndorserBlockHash`
-- and a given `NonPersistentVoter`. If the vote is valid, this function returns the `Weight`
-- that is associated with this vote.
verifyNonPersistentVote ::
  CommitteeSelection ->
  NonPersistentVoter ->
  ElectionId ->
  EndorserBlockHash ->
  PraosNonce ->
  NonPersistentVote ->
  Either String Weight
verifyNonPersistentVote CommitteeSelection {nonPersistentVoters, nonPersistentSeats} voter eId ebHash nonce vote
  | eId /= npvElectionId vote =
      Left "verifyNonPersistentVote: election ID in vote does not match input election ID"
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
      -- Check if the voter won at least one seat via local sortition
      seats <- case checkLeaderValueLeios
        vrfOutput
        (stakeNonPersistentVoter voter)
        (fromIntegral @NonPersistentSeats @Integer nonPersistentSeats) of
        Left err -> Left $ "verifyNonPersistentVote: local sortition failed: " ++ show err
        Right s -> Right s
      if seats >= 1
        then Right $ (fromIntegral @Int @Integer seats % 1) * weightPerNonPersistentSeat nonPersistentVoters
        else Left "verifyNonPersistentVote: voter did not win any seats in local sortition"

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
createNonPersistentVote nonce CommitteeSelection {nonPersistentVoters, nonPersistentSeats} privKey@(PrivateKeyLeios (nId, sk)) eId ebHash = case findNonPersistentVoterByPublicKey pk vtrs of
  Nothing -> Left "createNonPersistentVote: no non-persistent voter found for this key"
  Just (pId, npv) ->
    let npvRelativeStake = stakeNonPersistentVoter npv
        outputVRF =
          signWithRoleLeios
            (serialiseToRawBytes nonce <> writeBinaryWord64 eId)
            (coercePrivateKeyLeios privKey)
     in case checkLeaderValueLeios
          outputVRF
          npvRelativeStake
          (fromIntegral @NonPersistentSeats @Integer nonPersistentSeats) of
          Left err -> Left $ "createNonPersistentVote: local sortition failed: " ++ show err
          Right seats
            | seats >= 1 ->
                Right $
                  NonPersistentVote
                    { npvElectionId = eId
                    , npvPoolId = pId
                    , npvEligibilitySignature = outputVRF
                    , npvEndorserBlockHash = ebHash
                    , npvVoteSignature = signWithRoleLeios ebHash privKey
                    }
            | otherwise -> Left "createNonPersistentVote: voter did not win any seats in local sortition"
  where
    vtrs = voters nonPersistentVoters
    pk = PublicKeyLeios (nId, deriveVerKeyDSIGN sk) :: PublicKeyLeios 'Vote
