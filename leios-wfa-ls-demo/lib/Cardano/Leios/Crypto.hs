module Cardano.Leios.Crypto (
  PrivateVoteKey,
  PublicVoteKey,
) where

import Cardano.Crypto.DSIGN

-- The private voting key that a SPO uses to issue votes.
type PrivateVoteKey = SignKeyDSIGN BLS12381MinSigDSIGN

-- The public voting key that can be used to verify a SPO vote
type PublicVoteKey = VerKeyDSIGN BLS12381MinSigDSIGN

-- TODO: Add vote type (note that persistent votes are different then non-persistent)
-- TODD: The VRF output + elidibility proofs
