module Test.Cardano.Leios.NonIntegral (tests) where

import Cardano.Leios.NonIntegral (CompareResult (..), taylorExpCmp, taylorExpCmpFirstNonLower)
import Data.List (findIndex)
import Data.Ratio ((%))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  QuickCheckTests (..),
  chooseInt,
  chooseInteger,
  forAllBlind,
  testProperty,
  vectorOf,
 )

tests :: TestTree
tests =
  testGroup
    "NonIntegral"
    [ localOption (QuickCheckTests 1000) $
        testProperty
          "taylorExpCmpFirstNonLower matches individual taylorExpCmp calls"
          prop_taylorExpCmpFirstNonLowerMatches
    ]

-- | Property test verifying that taylorExpCmpFirstNonLower matches the behavior
-- of applying taylorExpCmp individually to each threshold and finding the first
-- non-BELOW result.
--
-- This tests that the optimized list version (which reuses Taylor series state)
-- produces the same result as calling the singular version on each element.
--
-- IMPORTANT: boundX must be e^{|x|} for the error bounds to be correct!
prop_taylorExpCmpFirstNonLowerMatches :: Property
prop_taylorExpCmpFirstNonLowerMatches =
  forAllBlind genTaylorExpCmpInputs $ \(boundX, cmps, x) ->
    let resultList = taylorExpCmpFirstNonLower boundX cmps x
        -- Apply taylorExpCmp to each threshold individually
        individualResults = [taylorExpCmp boundX cmp x | cmp <- cmps]
        -- Find the first result that is not BELOW
        resultIndividual = findIndex (not . isBELOW) individualResults
     in resultList == resultIndividual

-- | Check if a CompareResult is BELOW
isBELOW :: CompareResult a -> Bool
isBELOW (BELOW _ _) = True
isBELOW _ = False

-- | Generator for taylorExpCmp test inputs.
-- Generates:
--   - x: the exponent input
--   - boundX: should be e^{|x|} for the error bound to be mathematically correct
--   - cmps: a list of 0-20 comparison thresholds (between 0.01 and 100)
genTaylorExpCmpInputs :: Gen (Rational, [Rational], Rational)
genTaylorExpCmpInputs = do
  -- Generate x within a reasonable range
  xNum <- chooseInteger (-50, 50)
  xDenom <- chooseInteger (1, 10)
  let x = xNum % xDenom
      -- boundX should be e^{|x|} for correct error estimation
      -- Convert to rational with sufficient precision
      boundX = toRational (exp (fromRational (abs x) :: Double))

  -- Generate a list of thresholds
  numThresholds <- chooseInt (0, 20)
  cmps <- vectorOf numThresholds genThreshold

  pure (boundX, cmps, x)

-- | Generator for threshold values
genThreshold :: Gen Rational
genThreshold = do
  num <- chooseInteger (1, 1000)
  denom <- chooseInteger (1, 100)
  pure (num % denom)
