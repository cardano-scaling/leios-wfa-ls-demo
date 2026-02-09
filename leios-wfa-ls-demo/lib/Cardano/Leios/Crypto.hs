{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
  Stake,
  EndorserBlockHash,
  checkVoteSignature,
  checkEligibilitySignature,
) where

import Cardano.Crypto.DSIGN

import Cardano.Crypto.Util (writeBinaryWord64)
import Cardano.Leios.Types
import Data.Ratio ((%))

leiosBLSContext :: BLS12381SignContext
leiosBLSContext =
  BLS12381SignContext
    { blsSignContextDst = Just "LeiosV1"
    , blsSignContextAug = Nothing
    }

-- | The private voting key that a SPO uses to issue votes.
type PrivateVoteKey = SignKeyDSIGN BLS12381MinSigDSIGN

-- | The public voting key that can be used to verify a SPO vote
type PublicVoteKey = VerKeyDSIGN BLS12381MinSigDSIGN

type Vote = SigDSIGN BLS12381MinSigDSIGN

type PrivateVRFKey = SignKeyDSIGN BLS12381MinSigDSIGN

type PublicVRFKey = VerKeyDSIGN BLS12381MinSigDSIGN

type OutputVRF = SigDSIGN BLS12381MinSigDSIGN

checkVoteSignature :: PublicVoteKey -> EndorserBlockHash -> Vote -> Either String ()
checkVoteSignature = verifyDSIGN leiosBLSContext

checkEligibilitySignature ::
  PublicVRFKey -> Rational -> ElectionID -> OutputVRF -> Either String Weight
checkEligibilitySignature verKey stake eID output = do
  -- First we check that the BLS signature is valid
  verifyDSIGN leiosBLSContext verKey (writeBinaryWord64 eID) output
  -- Secondly we check that the output, seen as random variable
  -- in a uniform distribution is low enough and return the number of
  -- seats in weight
  checkVRFThreshold stake output

-- | Place holder vrf check
checkVRFThreshold :: Stake -> OutputVRF -> Either String Weight
checkVRFThreshold stake _output
  | stake > 1 % 3 = Left "Stake to large"
  | otherwise = Right (1 % 2)
