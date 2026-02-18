{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Leios.Crypto (
  Vote,
  ElectionID,
  OutputVRF,
  Weight,
  RelativeStake,
  EndorserBlockHash,
  KeyRoleLeios (..),
  PublicKeyLeios (..),
  PrivateKeyLeios (..),
  checkVRFThreshold,
  verifyPossessionProofLeios,
  createPossessionProofLeios,
  coercePrivateKeyLeios,
  coercePublicKeyLeios,
  HasBLSContext (..),
  verifyWithRoleLeios,
  signWithRoleLeios,
) where

import Cardano.Crypto.DSIGN

import Cardano.Api.Ledger (KeyHash (..))
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Leios.Types
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Ratio ((%))
import Numeric.Natural (Natural)

-- In Linear Leios we use BLS signature key material in three different ways.
--
-- 1) At registration, to issue a proof of possesion
-- 2) As a voter to endorse an EB
-- 3) As a VRF key, to check eligibility to become a non-persistent voter
--
-- Each underlying signature is a BLS signature, but each is seperated by
-- its usage context, and with it its signing context.

-- | Leios has three key roles, either use a key for Voting, a VRF, or to issue a PoP.
data KeyRoleLeios = Vote | VRF | PoP

-- | The private key of a Leios key pair
newtype PrivateKeyLeios (r :: KeyRoleLeios)
  = PrivateKeyLeios (SignKeyDSIGN BLS12381MinSigDSIGN)
  deriving newtype (Eq, Show)

-- | The public key of a Leios key pair
newtype PublicKeyLeios (r :: KeyRoleLeios)
  = PublicKeyLeios (VerKeyDSIGN BLS12381MinSigDSIGN)
  deriving newtype (Eq, Show)

newtype SignatureLeios (r :: KeyRoleLeios)
  = SignatureLeios (SigDSIGN BLS12381MinSigDSIGN)
  deriving newtype (Eq, Show)

-- | The vote of a `Party` that signals endorsment with an EB
type Vote = SignatureLeios 'Vote

-- | The VRF output that is used to deterministicly let a voting party
-- pick a random uniform distr. value between 0 and `2^384`.
type OutputVRF = SignatureLeios 'VRF

-- | The proof of possesion of any `PublicVoteKeyLeios r`
type PublicKeyPossessionProofLeios = PossessionProofDSIGN BLS12381MinSigDSIGN

coercePrivateKeyLeios :: PrivateKeyLeios r1 -> PrivateKeyLeios r2
coercePrivateKeyLeios = coerce

coercePublicKeyLeios :: PublicKeyLeios r1 -> PublicKeyLeios r2
coercePublicKeyLeios = coerce

-- Basic over G1: https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-06.html#section-4.2.1-1
minSigSignatureDST :: BLS12381SignContext
minSigSignatureDST = BLS12381SignContext (Just "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_") Nothing

-- PoP over G1: https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-06.html#section-4.2.3-1
minSigPoPDST :: BLS12381SignContext
minSigPoPDST = BLS12381SignContext (Just "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_POP_") Nothing

class HasBLSContext (r :: KeyRoleLeios) where
  blsCtx :: Proxy r -> BLS12381SignContext

-- TODO: make this polymorphic over network magic
instance HasBLSContext 'Vote where
  blsCtx _ = minSigSignatureDST {blsSignContextAug = Just "LEIOS:VOTE:MAINNET:V0:"}

instance HasBLSContext 'VRF where
  blsCtx _ = minSigSignatureDST {blsSignContextAug = Just "LEIOS:VRF:MAINNET:V0:"}

instance HasBLSContext 'PoP where
  blsCtx _ = minSigPoPDST {blsSignContextAug = Just "LEIOS:POP:MAINNET:V0:"}

signWithRoleLeios ::
  forall r msg.
  ( HasBLSContext r
  , SignableRepresentation msg
  ) =>
  msg -> PrivateKeyLeios r -> SignatureLeios r
signWithRoleLeios msg (PrivateKeyLeios sk) =
  SignatureLeios (signDSIGN (blsCtx (Proxy @r)) msg sk)

verifyWithRoleLeios ::
  forall r msg.
  ( HasBLSContext r
  , SignableRepresentation msg
  ) =>
  PublicKeyLeios r -> msg -> SignatureLeios r -> Either String ()
verifyWithRoleLeios (PublicKeyLeios vk) msg (SignatureLeios sig) =
  verifyDSIGN (blsCtx (Proxy @r)) vk msg sig

-- | Create a Proof of Possesion for a pool with `PoolID` and a given `PrivateKeyLeios 'PoP`
-- The binding to the pool id ensures that others cannot replay this PoP in their registration.
createPossessionProofLeios :: PrivateKeyLeios 'PoP -> PoolID -> PublicKeyPossessionProofLeios
createPossessionProofLeios (PrivateKeyLeios sk) pID = createPossessionProofDSIGN ctx' sk
  where
    ctx = blsCtx (Proxy @'PoP)
    ctx' = ctx {blsSignContextAug = blsSignContextAug ctx <> Just ((hashToBytes . unKeyHash) pID)}

verifyPossessionProofLeios ::
  PublicKeyLeios 'PoP -> PoolID -> PublicKeyPossessionProofLeios -> Either String ()
verifyPossessionProofLeios (PublicKeyLeios vk) pID = verifyPossessionProofDSIGN ctx' vk
  where
    ctx = blsCtx (Proxy @'PoP)
    ctx' = ctx {blsSignContextAug = blsSignContextAug ctx <> Just ((hashToBytes . unKeyHash) pID)}

-- | Place holder vrf check
checkVRFThreshold :: RelativeStake -> OutputVRF -> Either String Weight
checkVRFThreshold stake _output
  | stake > 1 % 3 = Left msg
  | otherwise = Right (1 % fromIntegral @Natural @Integer vrfMaxValue)
  where
    msg = "Stake to large"
    vrfMaxValue :: Natural
    vrfMaxValue = (2 :: Natural) ^ (8 * sizeSigDSIGN (Proxy @BLS12381MinSigDSIGN))
