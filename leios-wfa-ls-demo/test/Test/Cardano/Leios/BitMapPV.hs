module Test.Cardano.Leios.BitMapPV (tests) where

import Cardano.Binary (decodeFull, serialize')
import Cardano.Leios.BitMapPV (
  bitmapFromIndexes,
  bitmapSize,
  getAllFlippedIndexes,
  rawDeserialiseBitMapPV,
  rawSerialiseBitMapPV,
 )
import Cardano.Leios.WeightedFaitAccompli (PersistentVoterIndex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub, sort)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  QuickCheckTests (..),
  choose,
  counterexample,
  forAll,
  suchThat,
  testProperty,
  vectorOf,
  (===),
 )

tests :: TestTree
tests =
  testGroup
    "BitMapPV"
    [ localOption (QuickCheckTests 1000) $
        testProperty
          "roundtrip: getAllFlippedIndexes . bitmapFromIndexes = id"
          prop_roundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "duplicate indexes are idempotent"
          prop_duplicates_idempotent
    , localOption (QuickCheckTests 1000) $
        testProperty
          "out of bounds indexes are ignored"
          prop_out_of_bounds_ignored
    , localOption (QuickCheckTests 1000) $
        testProperty
          "bitmap size is correct"
          prop_bitmap_size
    , localOption (QuickCheckTests 1000) $
        testProperty
          "empty bitmap works correctly"
          prop_empty_bitmap
    , localOption (QuickCheckTests 1000) $
        testProperty
          "all bits set works correctly"
          prop_all_bits_set
    , localOption (QuickCheckTests 1000) $
        testProperty
          "raw serialization roundtrip"
          prop_raw_serialization_roundtrip
    , localOption (QuickCheckTests 1000) $
        testProperty
          "CBOR serialization roundtrip"
          prop_cbor_serialization_roundtrip
    ]

-- | Generator for valid bitmap test cases
-- Generates a list of indexes and a maximum index >= all indexes in the list
data BitmapTestCase = BitmapTestCase
  { btcIndexes :: [PersistentVoterIndex]
  , btcMaxIndex :: PersistentVoterIndex
  }
  deriving (Show)

instance Arbitrary BitmapTestCase where
  arbitrary = do
    maxIdx <- arbitrary `suchThat` (>= 0)
    numIndexes <- chooseInt (0, min 100 (fromIntegral maxIdx + 1))
    indexes <- vectorOf numIndexes (chooseInteger (0 :: Integer, fromIntegral maxIdx))
    return $ BitmapTestCase indexes maxIdx

-- | Property: Creating a bitmap from indexes and then extracting all flipped indexes
-- should return the original set of indexes (deduplicated and sorted)
prop_roundtrip :: Property
prop_roundtrip = forAll arbitrary $ \(BitmapTestCase indexes maxIdx) ->
  let bitmap = bitmapFromIndexes indexes maxIdx
      extracted = getAllFlippedIndexes bitmap
      -- Normalize the input by removing duplicates, filtering valid indexes, and sorting
      normalized = sort $ nub $ filter (<= maxIdx) indexes
   in counterexample
        ( "Input indexes: "
            ++ show indexes
            ++ "\nMax index: "
            ++ show maxIdx
            ++ "\nNormalized input: "
            ++ show normalized
            ++ "\nExtracted indexes: "
            ++ show extracted
        )
        $ extracted === normalized

-- | Property: Setting the same index multiple times should be the same as setting it once
prop_duplicates_idempotent :: Property
prop_duplicates_idempotent = forAll arbitrary $ \(BitmapTestCase indexes maxIdx) ->
  let
    -- Create bitmap with duplicates
    indexesWithDuplicates = indexes ++ indexes
    bitmapWithDuplicates = bitmapFromIndexes indexesWithDuplicates maxIdx
    -- Create bitmap without duplicates
    bitmapWithoutDuplicates = bitmapFromIndexes indexes maxIdx
    extractedWithDuplicates = getAllFlippedIndexes bitmapWithDuplicates
    extractedWithoutDuplicates = getAllFlippedIndexes bitmapWithoutDuplicates
   in
    counterexample
      ( "Input indexes: "
          ++ show indexes
          ++ "\nMax index: "
          ++ show maxIdx
          ++ "\nWith duplicates: "
          ++ show extractedWithDuplicates
          ++ "\nWithout duplicates: "
          ++ show extractedWithoutDuplicates
      )
      $ extractedWithDuplicates === extractedWithoutDuplicates

-- | Property: Indexes outside the valid range [0, maxIx] should be silently ignored
prop_out_of_bounds_ignored :: Property
prop_out_of_bounds_ignored = forAll genOutOfBoundsTestCase $ \(validIndexes, outOfBoundsIndexes, maxIdx) ->
  let allIndexes = validIndexes ++ outOfBoundsIndexes
      bitmap = bitmapFromIndexes allIndexes maxIdx
      extracted = getAllFlippedIndexes bitmap
      expected = sort $ nub validIndexes
   in counterexample
        ( "Valid indexes: "
            ++ show validIndexes
            ++ "\nOut of bounds indexes: "
            ++ show outOfBoundsIndexes
            ++ "\nMax index: "
            ++ show maxIdx
            ++ "\nAll indexes: "
            ++ show allIndexes
            ++ "\nExtracted: "
            ++ show extracted
            ++ "\nExpected: "
            ++ show expected
        )
        $ extracted === expected

-- | Generator for out-of-bounds test cases
genOutOfBoundsTestCase :: Gen ([PersistentVoterIndex], [PersistentVoterIndex], PersistentVoterIndex)
genOutOfBoundsTestCase = do
  maxIdx <- (fromIntegral :: Integer -> PersistentVoterIndex) <$> chooseInteger (10 :: Integer, 100)
  numValid <- chooseInt (0, 20)
  numOutOfBounds <- chooseInt (1, 10)
  validIndexes <-
    vectorOf
      numValid
      ( (fromIntegral :: Integer -> PersistentVoterIndex)
          <$> chooseInteger (0 :: Integer, fromIntegral maxIdx)
      )
  outOfBoundsIndexes <-
    vectorOf
      numOutOfBounds
      ( (fromIntegral :: Integer -> PersistentVoterIndex)
          <$> chooseInteger ((fromIntegral maxIdx + 1) :: Integer, (fromIntegral maxIdx + 100) :: Integer)
      )
  return (validIndexes, outOfBoundsIndexes, maxIdx)

-- | Property: The bitmap size in bytes should be (maxIndex / 8) + 1
prop_bitmap_size :: Property
prop_bitmap_size = forAll arbitrary $ \(BitmapTestCase indexes maxIdx) ->
  let bitmap = bitmapFromIndexes indexes maxIdx
      expectedSize = (fromIntegral maxIdx `quot` 8) + 1
      actualSize = bitmapSize bitmap
   in counterexample
        ( "Max index: "
            ++ show maxIdx
            ++ "\nExpected size (bytes): "
            ++ show expectedSize
            ++ "\nActual size (bytes): "
            ++ show actualSize
        )
        $ actualSize === expectedSize

-- | Property: An empty bitmap (no indexes set) should return an empty list
prop_empty_bitmap :: Property
prop_empty_bitmap = forAll (chooseInteger (0 :: Integer, 100)) $ \maxIdx ->
  let bitmap = bitmapFromIndexes [] maxIdx
      extracted = getAllFlippedIndexes bitmap
   in counterexample
        ( "Max index: "
            ++ show maxIdx
            ++ "\nExtracted indexes: "
            ++ show extracted
        )
        $ extracted === []

-- | Property: Setting all bits in range [0, maxIdx] should return all those indexes
prop_all_bits_set :: Property
prop_all_bits_set = forAll (chooseInteger (0 :: Integer, 100)) $ \maxIdx ->
  let allIndexes = [0 .. maxIdx]
      bitmap = bitmapFromIndexes allIndexes maxIdx
      extracted = getAllFlippedIndexes bitmap
   in counterexample
        ( "Max index: "
            ++ show maxIdx
            ++ "\nAll indexes: "
            ++ show allIndexes
            ++ "\nExtracted indexes: "
            ++ show extracted
        )
        $ extracted === allIndexes

-- | Property: Raw serialization roundtrip preserves the bitmap
prop_raw_serialization_roundtrip :: Property
prop_raw_serialization_roundtrip = forAll arbitrary $ \(BitmapTestCase indexes maxIdx) ->
  let bitmap = bitmapFromIndexes indexes maxIdx
      serialized = rawSerialiseBitMapPV bitmap
      deserialized = rawDeserialiseBitMapPV maxIdx serialized
      originalBits = getAllFlippedIndexes bitmap
   in counterexample
        ( "Original bitmap: "
            ++ show bitmap
            ++ "\nSerialized size: "
            ++ show (BS.length serialized)
            ++ "\nMax index: "
            ++ show maxIdx
            ++ "\nDeserialized: "
            ++ show deserialized
            ++ "\nOriginal bits: "
            ++ show originalBits
        )
        $ case deserialized of
          Nothing -> counterexample "Deserialization failed" False
          Just bitmap' ->
            let decodedBits = getAllFlippedIndexes bitmap'
             in counterexample ("Decoded bits: " ++ show decodedBits) $
                  decodedBits === originalBits

-- | Property: CBOR serialization roundtrip preserves the bitmap
prop_cbor_serialization_roundtrip :: Property
prop_cbor_serialization_roundtrip = forAll arbitrary $ \(BitmapTestCase indexes maxIdx) ->
  let bitmap = bitmapFromIndexes indexes maxIdx
      encoded = BSL.fromStrict $ serialize' bitmap
      decoded = decodeFull encoded
   in counterexample
        ( "Original bitmap: "
            ++ show bitmap
            ++ "\nMax index: "
            ++ show maxIdx
            ++ "\nEncoded size: "
            ++ show (BSL.length encoded)
            ++ "\nDecoded: "
            ++ show decoded
        )
        $ case decoded of
          Left err -> counterexample ("Decode error: " ++ show err) False
          Right bitmap' ->
            -- Compare the bitmaps directly - CBOR stores maxIdx so it should round-trip exactly
            let originalBits = getAllFlippedIndexes bitmap
                decodedBits = getAllFlippedIndexes bitmap'
             in counterexample
                  ( "\nOriginal bits: "
                      ++ show originalBits
                      ++ "\nDecoded bits: "
                      ++ show decodedBits
                  )
                  $ originalBits === decodedBits

-- Helper functions for QuickCheck generators
chooseInt :: (Int, Int) -> Gen Int
chooseInt = chooseInteger

chooseInteger :: (Integral a, Integral b) => (a, a) -> Gen b
chooseInteger (lo, hi) = fromIntegral <$> choose (fromIntegral lo :: Integer, fromIntegral hi :: Integer)


