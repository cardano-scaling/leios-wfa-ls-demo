{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Utils where

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Hashes (KeyHash (..))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Numeric.Natural (Natural)
import Text.Printf (printf)

import Cardano.Leios.Committee (OrderedSetOfParties (..), Party (..))
import Cardano.Leios.Types (PoolId)

-- | Write the ordered stake distribution to a CSV for plotting.
-- Also takes the file path, the number of persistent voters,
-- and the committee size for plotting.
writeStakeCSV :: FilePath -> Int -> Int -> OrderedSetOfParties -> IO ()
writeStakeCSV out persistentCount committeeSize OrderedSetOfParties {parties} = do
  let header =
        T.pack (printf "#persistent=%d\n" persistentCount)
          <> T.pack (printf "#committee=%d\n" committeeSize)
          <> T.pack "rank,stake\n"

      rows =
        [ T.pack (printf "%d,%.18f\n" (i :: Int) (fromRational (stake p) :: Double))
        | (i, p) <- zip [1 ..] parties
        ]

  T.writeFile out (header <> mconcat rows)

-- | Write the stake distribution to a CBOR file as a mapping of PoolId (hex) -> Natural.
-- The stakes (which sum to 1) are scaled by the given scaling factor to convert them
-- to natural numbers. For example, with scalingFactor = 1000000, a stake of 0.003860
-- would be represented as 3860.
writeStakeCBOR :: FilePath -> Natural -> OrderedSetOfParties -> IO ()
writeStakeCBOR out scalingFactor OrderedSetOfParties {parties} = do
  let stakeMap = buildStakeMap parties
  BSL.writeFile out $ CBOR.toLazyByteString $ encodeStakeMap stakeMap
  where
    buildStakeMap :: [Party] -> Map.Map T.Text Natural
    buildStakeMap ps =
      Map.fromList
        [ (poolIdToHex (poolId p), rationalToScaledNatural (stake p))
        | p <- ps
        ]

    rationalToScaledNatural :: Rational -> Natural
    rationalToScaledNatural r =
      let scaled = fromRational r * fromIntegral scalingFactor :: Double
       in round scaled

    poolIdToHex :: PoolId -> T.Text
    poolIdToHex =
      T.decodeUtf8 . Base16.encode . hashToBytes . unKeyHash

    encodeStakeMap :: Map.Map T.Text Natural -> CBOR.Encoding
    encodeStakeMap m =
      CBOR.encodeMapLen (fromIntegral $ Map.size m)
        <> mconcat
          [ CBOR.encodeString key <> CBOR.encodeWord64 (fromIntegral val)
          | (key, val) <- Map.toList m
          ]
