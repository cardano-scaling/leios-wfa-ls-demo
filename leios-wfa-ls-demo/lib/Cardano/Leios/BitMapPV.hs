{-# LANGUAGE BangPatterns #-}

module Cardano.Leios.BitMapPV (
  BitMapPV,
  bitmapFromIndexes,
  getAllFlippedIndexes,
  bitmapSize,
  rawSerialiseBitMapPV,
  rawDeserialiseBitMapPV,
) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Leios.WeightedFaitAccompli (PersistentVoterIndex)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad (forM_, when)
import Data.Bits (
  countTrailingZeros,
  popCount,
  unsafeShiftL,
  (.&.),
  (.|.),
 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word16, Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (peekByteOff, pokeByteOff)

-- | A compact bitmap representation for tracking voter participation or binary flags.
-- The logical upper bound is stored explicitly so serialization round-trips exactly.
data BitMapPV = BitMapPV !PersistentVoterIndex !BS.ByteString
  deriving (Eq)

instance Show BitMapPV where
  show (BitMapPV maxIx bs) =
    "BitMapPV{maxIx="
      ++ show maxIx
      ++ ",bytes="
      ++ show (BS.length bs)
      ++ ",set="
      ++ show (countSetBits bs)
      ++ "}"
    where
      countSetBits arr =
        sum [popCount (BS.index arr i) | i <- [0 .. BS.length arr - 1]]

bitmapFromIndexes ::
  [PersistentVoterIndex] ->
  PersistentVoterIndex ->
  BitMapPV
bitmapFromIndexes flipped maxIx =
  BitMapPV maxIx $
    BSI.unsafeCreate nBytes $ \ptr -> do
      fillBytes ptr 0 nBytes
      forM_ flipped $ \ix -> do
        let !i = fromIntegral ix :: Int
        when (i >= 0 && i <= maxI) $ do
          let !byteIx = i `quot` 8
              !bitIx = i `rem` 8
              !mask = bitMask bitIx
          w <- peekByteOff ptr byteIx :: IO Word8
          pokeByteOff ptr byteIx (w .|. mask)
  where
    !maxI = fromIntegral maxIx :: Int
    !nBytes = (maxI `quot` 8) + 1

getAllFlippedIndexes :: BitMapPV -> [PersistentVoterIndex]
getAllFlippedIndexes (BitMapPV maxIx bitmap) =
  goBytes 0
  where
    !maxI = fromIntegral maxIx :: Int
    !nBytes = BS.length bitmap

    goBytes !byteIx
      | byteIx >= nBytes = []
      | otherwise =
          let !w = BS.index bitmap byteIx
           in goBits (byteIx * 8) w ++ goBytes (byteIx + 1)

    goBits !_ 0 = []
    goBits !base !w =
      let !bitIx = countTrailingZeros w
          !i = base + bitIx
          !w' = w .&. (w - 1)
       in if i <= maxI
            then fromIntegral i : goBits base w'
            else []

bitmapSize :: BitMapPV -> Int
bitmapSize (BitMapPV _ bs) = BS.length bs

bitMask :: Int -> Word8
bitMask k = fromIntegral ((1 :: Int) `unsafeShiftL` k)

rawSerialiseBitMapPV :: BitMapPV -> BS.ByteString
rawSerialiseBitMapPV (BitMapPV _ bs) = bs

rawDeserialiseBitMapPV :: PersistentVoterIndex -> BS.ByteString -> Maybe BitMapPV
rawDeserialiseBitMapPV maxIx bs
  | BS.length bs /= expectedBytes = Nothing
  | otherwise = Just (BitMapPV maxIx bs)
  where
    expectedBytes = (fromIntegral maxIx `quot` 8) + 1

instance ToCBOR BitMapPV where
  toCBOR (BitMapPV maxIx bs) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord16 (fromIntegral maxIx :: Word16)
      <> CBOR.encodeBytes bs

instance FromCBOR BitMapPV where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    maxIx <- CBOR.decodeWord16
    bs <- CBOR.decodeBytes
    case rawDeserialiseBitMapPV maxIx bs of
      Nothing -> fail "BitMapPV: invalid bitmap data or size mismatch"
      Just bitmap -> pure bitmap
