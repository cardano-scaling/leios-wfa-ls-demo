module Cardano.Leios.VoteAccumulator (
  -- * Vote accumulation
  VoteAccumulator (..),
  emptyVoteAccumulator,
  addValidatedVote,
  validateAndAddVote,
  hasReachedThreshold,
  getAccumulatorWeight,

  -- * Certificate selection tactics
  CertificateSelectionTactic (..),
  selectVotesForCertificate,
  -- createCertificateFromAccumulator,
) where

-- import Cardano.Leios.Certificate (Certificate, createCertificate)
import Cardano.Leios.Types (ElectionId, EndorserBlockHash, PoolId, Weight)
import Cardano.Leios.Vote (
  LeiosVote (..),
  NonPersistentVote (..),
  PersistentVote (..),
  verifyLeiosVote,
 )
import Cardano.Leios.WeightedFaitAccompli (
  CommitteeSelection,
  PersistentVoterIndex,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Vote Accumulator for incremental vote collection
--------------------------------------------------------------------------------

-- | Accumulator for validated votes with their weights
-- Supports incremental addition of votes as they arrive asynchronously
data VoteAccumulator = VoteAccumulator
  { pvVotes :: Map PersistentVoterIndex (PersistentVote, Weight)
  , npvVotes :: Map PoolId (NonPersistentVote, Weight)
  , totalWeight :: Weight
  }
  deriving (Eq, Show)

-- | Create an empty vote accumulator
emptyVoteAccumulator :: VoteAccumulator
emptyVoteAccumulator = VoteAccumulator Map.empty Map.empty 0

-- | Add a validated vote with its weight to the accumulator
-- Silently ignores duplicate votes (same voter ID)
addValidatedVote :: LeiosVote -> Weight -> VoteAccumulator -> VoteAccumulator
addValidatedVote vote weight acc = case vote of
  LeiosPersistentVote pv ->
    let pvId = pvPersistentVoterId pv
     in case Map.lookup pvId (pvVotes acc) of
          Nothing ->
            acc
              { pvVotes = Map.insert pvId (pv, weight) (pvVotes acc)
              , totalWeight = totalWeight acc + weight
              }
          Just _ -> acc -- Duplicate, ignore
  LeiosNonPersistentVote npv ->
    let poolId = npvPoolId npv
     in case Map.lookup poolId (npvVotes acc) of
          Nothing ->
            acc
              { npvVotes = Map.insert poolId (npv, weight) (npvVotes acc)
              , totalWeight = totalWeight acc + weight
              }
          Just _ -> acc -- Duplicate, ignore

-- | Check if the accumulator has reached a weight threshold
hasReachedThreshold :: Weight -> VoteAccumulator -> Bool
hasReachedThreshold threshold acc = totalWeight acc >= threshold

-- | Get the total weight in the accumulator
getAccumulatorWeight :: VoteAccumulator -> Weight
getAccumulatorWeight = totalWeight

-- | Validate a vote and add it to the accumulator in one step
-- Checks for duplicates first (skips expensive verification if vote already exists)
-- Verifies the vote against the committee, and if valid, adds it with its weight
-- Returns the updated accumulator or an error if validation fails
-- Silently ignores duplicates by returning the unchanged accumulator
validateAndAddVote ::
  CommitteeSelection ->
  ElectionId ->
  EndorserBlockHash ->
  LeiosVote ->
  VoteAccumulator ->
  Either String VoteAccumulator
validateAndAddVote committee eId ebHash vote acc =
  -- Check for duplicates first to avoid expensive verification
  if isDuplicate vote acc
    then Right acc -- Duplicate, return unchanged accumulator
    else do
      -- Not a duplicate, verify and add
      weight <- verifyLeiosVote committee eId ebHash vote
      Right $ addValidatedVote vote weight acc
  where
    isDuplicate :: LeiosVote -> VoteAccumulator -> Bool
    isDuplicate (LeiosPersistentVote pv) accumulator =
      Map.member (pvPersistentVoterId pv) (pvVotes accumulator)
    isDuplicate (LeiosNonPersistentVote npv) accumulator =
      Map.member (npvPoolId npv) (npvVotes accumulator)

--------------------------------------------------------------------------------
-- Certificate Selection Tactics
--------------------------------------------------------------------------------

-- | Selection tactics for choosing which votes to include in a certificate
data CertificateSelectionTactic
  = -- | Include all accumulated votes
    SelectAllVotes
  | -- | Randomly select until threshold is reached
    SelectRandomUntilThreshold Weight
  | -- | Select all PV votes first, then NPV until threshold
    SelectPVFirst Weight
  deriving (Eq, Show)

-- | Select votes from accumulator according to a tactic
-- Returns the selected votes and their total weight
selectVotesForCertificate ::
  CertificateSelectionTactic ->
  VoteAccumulator ->
  Either String ([LeiosVote], Weight)
selectVotesForCertificate tactic acc = case tactic of
  SelectAllVotes -> selectAllVotes acc
  SelectRandomUntilThreshold threshold -> selectRandomUntilThreshold threshold acc
  SelectPVFirst threshold -> selectPVFirst threshold acc

-- Select all votes from the accumulator
selectAllVotes :: VoteAccumulator -> Either String ([LeiosVote], Weight)
selectAllVotes acc =
  let pvs = map (\(pv, _) -> LeiosPersistentVote pv) $ Map.elems (pvVotes acc)
      npvs = map (\(npv, _) -> LeiosNonPersistentVote npv) $ Map.elems (npvVotes acc)
      allVotes = pvs ++ npvs
   in if null allVotes
        then Left "selectAllVotes: no votes in accumulator"
        else Right (allVotes, totalWeight acc)

-- Select all PV votes first, then add NPV votes until threshold is reached
selectPVFirst :: Weight -> VoteAccumulator -> Either String ([LeiosVote], Weight)
selectPVFirst threshold acc = do
  let pvList = Map.elems (pvVotes acc)
      npvList = Map.elems (npvVotes acc)
      pvVotesList = map (\(pv, _) -> LeiosPersistentVote pv) pvList
      pvWeight = sum (map snd pvList)

  if pvWeight >= threshold
    then Right (pvVotesList, pvWeight)
    else do
      -- Need to add some NPV votes
      let remainingWeight = threshold - pvWeight
          (selectedNPVs, npvWeight) = selectNPVUntilWeight remainingWeight npvList
      Right (pvVotesList ++ selectedNPVs, pvWeight + npvWeight)
  where
    selectNPVUntilWeight :: Weight -> [(NonPersistentVote, Weight)] -> ([LeiosVote], Weight)
    selectNPVUntilWeight _target [] = ([], 0)
    selectNPVUntilWeight target ((npv, w) : rest)
      | w >= target = ([LeiosNonPersistentVote npv], w)
      | otherwise =
          let (restVotes, restWeight) = selectNPVUntilWeight (target - w) rest
           in (LeiosNonPersistentVote npv : restVotes, w + restWeight)

-- Select random votes until threshold is reached
-- Note: This is a placeholder - proper implementation would need a random generator
selectRandomUntilThreshold :: Weight -> VoteAccumulator -> Either String ([LeiosVote], Weight)
selectRandomUntilThreshold =
  -- For now, just use the same strategy as SelectPVFirst
  -- A real implementation would need to take a RandomGen parameter
  -- or use IO to get random values
  selectPVFirst

--------------------------------------------------------------------------------
-- Certificate Creation from Accumulator
--------------------------------------------------------------------------------

-- -- | Create a certificate from the accumulator using a selection tactic
-- createCertificateFromAccumulator ::
--   ElectionId ->
--   EndorserBlockHash ->
--   CommitteeSelection ->
--   CertificateSelectionTactic ->
--   VoteAccumulator ->
--   Either String Certificate
-- createCertificateFromAccumulator eId ebHash committee tactic acc = do
--   (selectedVotes, _weight) <- selectVotesForCertificate tactic acc
--   createCertificate eId ebHash committee selectedVotes
