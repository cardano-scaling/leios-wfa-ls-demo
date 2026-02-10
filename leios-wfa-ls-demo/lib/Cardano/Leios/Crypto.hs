{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Leios.Crypto (
  PrivateVoteKey,
  PublicVoteKey,
  Vote,
  PrivateVRFKey,
  ElectionID,
  PublicVRFKey,
  OutputVRF,
  Weight,
  RelativeStake,
  EndorserBlockHash,
  checkVoteSignature,
  checkVRFOutput,
  checkVRFThreshold,
  createVRFOutput,
  createVoteSignature,
) where

import Cardano.Crypto.DSIGN

import Cardano.Crypto.Util (writeBinaryWord64)
import Cardano.Leios.Types
import Data.Data (Proxy (..))
import Data.Ratio ((%))
import Numeric.Natural (Natural)

-- | The BLS signature signing context used for Leios.
-- The domain seperation tag should be so that it is
-- unlikely that it is used elsewhere. Note that it
-- it is good practice to make this dependent on the
-- network magic tag.
leiosBLSContext :: BLS12381SignContext
leiosBLSContext =
  BLS12381SignContext
    { blsSignContextDst = Just "LeiosV1_Demo"
    , blsSignContextAug = Nothing
    }

-- | The private voting key that a SPO uses to issue votes.
type PrivateVoteKey = SignKeyDSIGN BLS12381MinSigDSIGN

-- | The public voting key that can be used to verify a SPO vote
type PublicVoteKey = VerKeyDSIGN BLS12381MinSigDSIGN

-- | The vote of a `Party` that signals agreement with an EB
type Vote = SigDSIGN BLS12381MinSigDSIGN

-- | The private VRF key, note that this is equal to the `PrivateVoteKey`
-- And though a different type is used for both, they same underlying
-- `SignKeyDSIGN BLS12381MinSigDSIGN` should be used for both.
type PrivateVRFKey = SignKeyDSIGN BLS12381MinSigDSIGN

-- | The public VRF key that can be used for verify eligibility proofs.
-- Similar to the `PrivateVRFKey`, this is the same as underlying object as
-- the `PublicVoteKey`, but used in a different context.
type PublicVRFKey = VerKeyDSIGN BLS12381MinSigDSIGN

-- | The VRF output that is used to deterministicly let a voting party
-- pick a random uniform distr. value between 0 and `2^384`.
type OutputVRF = SigDSIGN BLS12381MinSigDSIGN

-- | Verify that the `Vote` is valid over the `EndorserBlockHash` given a `PublicVoteKey`.
checkVoteSignature :: PublicVoteKey -> EndorserBlockHash -> Vote -> Either String ()
checkVoteSignature = verifyDSIGN leiosBLSContext

-- | Create an `Vote` given an `EndorserBlockHash` and a `PrivateVoteKey`
createVoteSignature :: EndorserBlockHash -> PrivateVoteKey -> Vote
createVoteSignature = signDSIGN leiosBLSContext

-- | Verify that the `OutputVRF` is valid over the `ElectionID` given a `PublicVRFKey`.
checkVRFOutput :: PublicVRFKey -> ElectionID -> OutputVRF -> Either String ()
checkVRFOutput vk eID = verifyDSIGN leiosBLSContext vk (writeBinaryWord64 eID)

-- | Create an `OutputVRF` value given an `ElectionID` and a `PrivateVRFKey`
createVRFOutput :: ElectionID -> PrivateVRFKey -> OutputVRF
createVRFOutput eID = signDSIGN leiosBLSContext (writeBinaryWord64 eID)

-- | Place holder vrf check
checkVRFThreshold :: RelativeStake -> OutputVRF -> Either String Weight
checkVRFThreshold stake _output
  | stake > 1 % 3 = Left msg
  | otherwise = Right (1 % fromIntegral @Natural @Integer vrfMaxValue)
  where
    msg = "Stake to large"
    vrfMaxValue :: Natural
    vrfMaxValue = (2 :: Natural) ^ (8 * sizeSigDSIGN (Proxy @BLS12381MinSigDSIGN))
