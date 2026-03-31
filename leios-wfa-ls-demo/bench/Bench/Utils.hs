module Bench.Utils (
  BenchEnv (..),
  testNetworkId,
  testEbHash,
  testElectionId,
  mkPoolId,
  mkPrivKey,
  mkPubKey,
  mkPVCommittee,
  mkNPVCommittee,
  randomBenchInputs,
  findWinningInput,
  findLosingInput,
  feasibleForLose,
  generateParetoStakeDist,
  generateLinearStakeDist,
) where

import Cardano.Api (NetworkId (..), NetworkMagic (..), PraosNonce)
import Cardano.Api.Shelley (makePraosNonce)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (KeyHash (..))
import Cardano.Leios.Crypto (PrivateKeyLeios (..), PublicKeyLeios (..))
import Cardano.Leios.Types (ElectionId, EndorserBlockHash, PoolId)
import Cardano.Leios.Utils (toSkForBLS, toVerKeyForBLS)
import Cardano.Leios.Vote (NonPersistentVote, createNonPersistentVote)
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection (..),
  NonPersistentLocalSortition (..),
  NonPersistentVoter (..),
  PersistentSeat (..),
 )
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Data.Ratio ((%))
import Data.Word (Word16, Word64)
import System.IO (IOMode (..), withBinaryFile)

-- | Opaque wrapper satisfying criterion's NFData constraint for perRunEnv.
-- Values returned from IO are already evaluated, so shallow seq suffices.
newtype BenchEnv a = BenchEnv {unBenchEnv :: a}

instance NFData (BenchEnv a) where
  rnf x = x `seq` ()

testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

testEbHash :: EndorserBlockHash
testEbHash = Hash.castHash $ Hash.hashWith id $ BSC.pack "test-endorser-block"

testElectionId :: ElectionId
testElectionId = 42

testNonce :: PraosNonce
testNonce = makePraosNonce $ BSC.pack "test-nonce"

mkPoolId :: Int -> PoolId
mkPoolId idx = KeyHash $ Hash.castHash $ Hash.hashWith id $ BSC.pack (show idx)

mkPrivKey :: PoolId -> PrivateKeyLeios
mkPrivKey pId = PrivateKeyLeios (testNetworkId, toSkForBLS pId)

mkPubKey :: PoolId -> PublicKeyLeios
mkPubKey pId = toVerKeyForBLS pId testNetworkId

-- | Build a CommitteeSelection with exactly one persistent seat for the given key.
-- Bypasses wFA so we control the seat directly.
mkPVCommittee :: PrivateKeyLeios -> CommitteeSelection
mkPVCommittee (PrivateKeyLeios (nId, sk)) =
  CommitteeSelection
    { persistentSeats =
        Map.fromList
          [
            ( 0
            , PersistentSeat
                { publicVoteKeyPersistent = PublicKeyLeios (nId, deriveVerKeyDSIGN sk)
                , weightPersistentSeat = 1 % 1
                }
            )
          ]
    , nonPersistentVoters =
        NonPersistentLocalSortition
          { voters = Map.empty
          , weightPerNonPersistentSeat = 0
          }
    , praosNonce = testNonce
    , targetCommitteeSize = 1
    , nonPersistentSeats = 0
    , networkId = nId
    }

-- | Build a CommitteeSelection with exactly one NPV voter with normalised stake σ and n2 seats.
-- Bypasses wFA so we control n2 and σ directly.
mkNPVCommittee :: Rational -> Word16 -> PoolId -> PublicKeyLeios -> CommitteeSelection
mkNPVCommittee sigma n2 pId pubKey =
  CommitteeSelection
    { persistentSeats = Map.empty
    , nonPersistentVoters =
        NonPersistentLocalSortition
          { voters =
              Map.fromList
                [
                  ( pId
                  , NonPersistentVoter
                      { publicVoteKeyNonPersistent = pubKey
                      , stakeNonPersistentVoter = sigma
                      }
                  )
                ]
          , weightPerNonPersistentSeat = 1 % 1
          }
    , praosNonce = testNonce
    , targetCommitteeSize = n2
    , nonPersistentSeats = n2
    , networkId = testNetworkId
    }

randomBytes :: Int -> IO BS.ByteString
randomBytes n = withBinaryFile "/dev/urandom" ReadMode $ \h -> BS.hGet h n

bytesToWord64 :: BS.ByteString -> Word64
bytesToWord64 = BS.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0

-- | Generate a fresh random (PraosNonce, ElectionId, EndorserBlockHash).
randomBenchInputs :: IO (PraosNonce, ElectionId, EndorserBlockHash)
randomBenchInputs = do
  nonceBytes <- randomBytes 32
  eIdBytes <- randomBytes 8
  hashBytes <- randomBytes 32
  let nonce = makePraosNonce nonceBytes
      eId = bytesToWord64 eIdBytes
      ebHash = Hash.castHash $ Hash.hashWith id hashBytes
  return (nonce, eId, ebHash)

-- | Scan random inputs until createNonPersistentVote returns Right (pool wins ≥1 seat).
findWinningInput ::
  CommitteeSelection ->
  PrivateKeyLeios ->
  IO (PraosNonce, ElectionId, EndorserBlockHash, NonPersistentVote)
findWinningInput cs privKey = go
  where
    go = do
      (nonce, eId, ebHash) <- randomBenchInputs
      case createNonPersistentVote nonce cs privKey eId ebHash of
        Right vote -> return (nonce, eId, ebHash, vote)
        Left _ -> go

-- | Scan random inputs until createNonPersistentVote returns Left (sortition failure).
-- Only call for (σ, n2) where λ = σ×n2 < 5 (see feasibleForLose).
findLosingInput ::
  CommitteeSelection ->
  PrivateKeyLeios ->
  IO (PraosNonce, ElectionId, EndorserBlockHash)
findLosingInput cs privKey = go
  where
    go = do
      (nonce, eId, ebHash) <- randomBenchInputs
      case createNonPersistentVote nonce cs privKey eId ebHash of
        Left _ -> return (nonce, eId, ebHash)
        Right _ -> go

-- | Only emit the create-lose bench when λ = σ×n2 < 5 (P(lose) > 0.7%).
feasibleForLose :: Rational -> Word16 -> Bool
feasibleForLose sigma n2 = sigma * fromIntegral n2 < 5

-- | Generate a synthetic stake distribution of @n@ pools drawn from a
-- Pareto(α, x_m=1) distribution and normalised to sum to exactly 1.
--
-- Uses the inverse-CDF transform: given U ~ Uniform(0,1),
--   X = 1 / (1 - U)^(1/α)
-- Smaller α → fatter tail → stake more concentrated in top pools.
-- Larger α → tail decays faster → distribution closer to uniform.
-- Pool ids are deterministic: 'mkPoolId' 0 .. n-1.
generateParetoStakeDist :: Double -> Int -> IO (Map.Map PoolId Rational)
generateParetoStakeDist alpha n = do
  us <- replicateM n randomUnit
  let xs = map (paretoSample alpha) us
      xsR = map toRational xs
      total = sum xsR
      stakes = map (/ total) xsR
  return $ Map.fromList (zip (map mkPoolId [0 .. n - 1]) stakes)
  where
    -- Draw one double uniformly in (0, 1) via /dev/urandom.
    -- We mask to 53 significant bits to stay within Double precision.
    randomUnit :: IO Double
    randomUnit = do
      bs <- randomBytes 8
      let w = bytesToWord64 bs
          mantissa = w `mod` (2 ^ (53 :: Int))
      return $ fromIntegral mantissa / fromIntegral (2 ^ (53 :: Int) :: Word64)
    paretoSample :: Double -> Double -> Double
    paretoSample a u = 1.0 / (1.0 - u) ** (1.0 / a)

-- | Generate a synthetic stake distribution of @n@ pools with linearly
-- decreasing stake: pool @i@ gets weight @(n - i)@, normalised to sum to 1.
-- Pool 0 has the highest stake, pool @n-1@ has the lowest.
generateLinearStakeDist :: Int -> Map.Map PoolId Rational
generateLinearStakeDist n =
  Map.fromList
    [ (mkPoolId i, fromIntegral (n - i) % total)
    | i <- [0 .. n - 1]
    ]
  where
    total = fromIntegral (n * (n + 1) `div` 2)
