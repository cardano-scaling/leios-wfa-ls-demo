module Cardano.Leios.Vote where

import Cardano.Leios.Committee (PoolID)
import Cardano.Leios.Crypto (OutputVRF, PublicVoteKey, Vote)
import Cardano.Leios.WeightedFaitAccompli (CommitteeSelection, PersistentSeat, PersistentVoterId)
import Data.Word (Word64)

-- | Identifier for the voting round (derived from the slot number of the RB that announced the target EB)
type ElectionID = Word64

data LeiosVote = LeiosPersistentVote | LeiosNonPersistentVote

data PersistentVote = PersistentVote
  { pvElectionID :: ElectionID
  , pvPersistentVoterID :: PersistentVoterId
  , pvEndorser_block_hash :: Integer
  , pvVoteSignature :: Vote
  }

data NonPersistentVote = NonPersistentVote
  { npvElectionID :: ElectionID
  , npvPoolID :: PoolID
  , npvEligibilitySignature :: OutputVRF
  , npvVoteSignature :: Vote
  }

-- | TODO: for either (non)-persistent vote
-- check that seat/PoolID in vote is in committee
-- then either call verify(Non)PersistentVote
verifyLeiosVote :: CommitteeSelection -> LeiosVote -> Bool
verifyLeiosVote _comm _vote = True

verifyPersistentVote :: PersistentSeat -> PersistentVote -> Bool
verifyPersistentVote _seat _vote = True

verifyNonPersistentVote :: PublicVoteKey -> NonPersistentVote -> Bool
verifyNonPersistentVote _publicKey _vote = True

-- castVote :: PrivateVoteKey ->
