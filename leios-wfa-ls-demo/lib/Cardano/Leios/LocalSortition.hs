{-# LANGUAGE TypeApplications #-}

module Cardano.Leios.LocalSortition where

import Cardano.Ledger.BaseTypes (FixedPoint)
import Cardano.Leios.Crypto
import Cardano.Leios.NonIntegral (FirstNonLowerError, taylorExpCmpFirstNonLower)
import Cardano.Protocol.TPraos.BHeader (BoundedNatural (..), assertBoundedNatural)
import GHC.Real ((%))
import Numeric.Natural ()

{-
Some notes on local sortition using a random variable `X ~ Poisson(\lambda)`

PRAOS: In praos, we want the target of 1 block for each 20 seconds. Which is why we have in the shelley genesis file

"activeSlotsCoeff": 0.05

At each slot, we have a lottery, where 1 lovelace = 1 ticket. This means that if we had one pool that had all active stake, it would
win each slot with probability f = 0.05 (or rate). That is, using a repeated Bernoulli trial

P("Pool with all stake wins") = 1 - P("Pool has no ticket that wins")
                              = 1 - (1-x)^totalActiveStake
                              = f

where x is the chance that one ticket wins. If we solve for x we get that

x = 1 - (1 - f)^(1/totalActiveStake)

Now in the case where stake is distributed, we can use this ideal definition of p via

P("Pool with stake n wins") = 1 - P("Pool has no ticket that wins")
                            = 1 - (1- x)^n
                            = 1 - (1 - (1 - (1 - f)^(1/totalActiveStake)) )^n
                            = 1 - (1 - f)^(n/totalActiveStake)

and if we define σ = (n / totalActiveStake), then we get

P("Pool with stake ratio σ wins") = 1 - (1 - f)^σ

Now using a VRF, which has the property that it can uniformly map into an interval,
we can assign each pool at each slot a random value p between zero and one.

Then we say that a pool wins at a slot if

p < 1 - (1 - f)^σ

that is; their normalised VRF output is lower than the probability that at least
one of their tickets win. If we further define q = 1 - p and c = ln(1 - f) then we have

p < 1 - (1 - f)^σ <=> 1 / (1 - p) < exp(-σ ln(1 - f)) <=> 1 / (1 - p) < exp(-σ * c) <=> 1 / q < exp(-σ * c)

where the latter exponent can be calculated to sufficient precision to decide the comparison
using a Taylor expansion. This is done to efficiently say when a pool is below their threshold. This is because we
only have to calculate up to a certain order of the expansion, as at some point the error
will be smaller than the current result and the threshold it needs to pass.

LEIOS: In leios the game is different, for each EB (which is linked to a slot), we want
on average that n2 non-persistent voters win the right to endorse an EB via a Poisson distribution.

Now since we have that for X_i ~ Poisson(λ_i) that E[X_i] = λ_i and that \Sum_i X_i ~ Poisson(\Sum λ_i).

Combine this with the fact that we desire that more stake wins you proportionally more seats, we should
aim for

E[X_totalSeats] = λ_total = E[\Sum_i X_i] = \Sum_i λ_i = n2

which is a similar constraint to the above x = 1 - (1 - f)^(1/totalActiveStake), it tells
us how to set λ_i.

Note that if we define λ_i = σ_i * n2 (where σ_i is the stake of each party), we get that

λ_total = Sum_i λ_i = Sum_i (σ_i * n2) = n2 * \Sum_i σ_i = n2

So, in our calculation we define for a single pool that holds σ stake that its rate is λ = σ * n2

To sample the number of seats from Poisson(λ), we use the inverse CDF method with VRF output p ∈ [0,1]:
We find k such that CDF(k-1) < p ≤ CDF(k), where

CDF(k) = P(X ≤ k) = ∑_{i=0}^{k} P(X=i) = e^(-λ) * ∑_{i=0}^{k} λ^i/i!

We can compute this iteratively. Define S[k] = ∑_{i=0}^{k} λ^i/i!, then:

S[0] = 1
S[k] = S[k-1] + λ^k/k!

where the term λ^k/k! can be computed from the previous term via:

term[0] = 1
term[k] = term[k-1] * λ/k

Then CDF(k) = e^(-λ) * S[k], and we check if p ≤ CDF(k), which is equivalent to:

p ≤ e^(-λ) * S[k]  ⟺  p * e^λ ≤ S[k]  ⟺  S[k] / p ≥ e^λ

So the iterative algorithm is:

k=0: S = 1,            check if 1/p           ≥ e^λ, if yes return 0
k=1: S = 1 + λ,        check if (1+λ)/p       ≥ e^λ, if yes return 1
k=2: S = 1 + λ + λ²/2, check if (1+λ+λ²/2)/p  ≥ e^λ, if yes return 2
...

where each S[k] is updated via S[k] = S[k-1] + term[k] with term[k] = term[k-1] * λ/k.

We iterate until we find the first k where p ≤ CDF(k), equivalently S[k]/p ≥ e^λ. Because S[k]/p
is increasing in k and we check in order, we find the smallest such k. This automatically ensures
CDF(k-1) < p ≤ CDF(k), correctly sampling from Poisson(λ).

Implementation detail:
We use taylorExpCmpFirstNonLower, which returns the first index i where orders[i] ≥ e^x.
In our case:
  - orders = [S[0]/p, S[1]/p, S[2]/p, ...] (increasing sequence)
  - x = λ
  - Returns: first k where S[k]/p ≥ e^λ

This gives us the correct inverse CDF:
  S[k]/p ≥ e^λ
  ⟺ S[k] ≥ p × e^λ
  ⟺ S[k] × e^(-λ) ≥ p
  ⟺ CDF(k) ≥ p

So we find min{k : CDF(k) ≥ p}, which is exactly the inverse CDF method for sampling from Poisson(λ).
The comparison is done efficiently using a Taylor expansion for e^λ.

-}

checkLeaderValueLeios ::
  OutputVRF ->
  Rational -> -- stake ratio σ
  Integer -> -- n2
  Either FirstNonLowerError Int -- The number of seats this node wins, or an error
checkLeaderValueLeios outputVRF σ n2 =
  checkLeaderNatValueLeios
    (assertBoundedNatural outputNatMax (getOutputVRFNatural outputVRF))
    σ
    (n2 % 1)

checkLeaderNatValueLeios ::
  BoundedNatural -> -- p
  Rational -> -- stake ratio σ
  Rational -> -- n2
  Either FirstNonLowerError Int
checkLeaderNatValueLeios bn σ n2
  | n2 <= 0 = Right 0
  | σ <= 0 = Right 0
  | otherwise = taylorExpCmpFirstNonLower 3 orders λ
  where
    λ, p :: FixedPoint
    λ = fromRational (n2 * σ)
    certNatMax = bvMaxValue bn
    certNat = bvValue bn
    -- Normalize VRF output to [0, 1]: p = certNat / certNatMax
    p = fromRational (toInteger certNat % toInteger certNatMax)

    -- Compute terms: [1, λ, λ²/2, λ³/6, ...] = [λ^k/k! for k=0,1,2,...]
    terms :: [FixedPoint]
    terms = 1 : zipWith (\k prev -> prev * λ / fromIntegral @Integer @FixedPoint k) [1 ..] terms

    -- Compute cumulative sums S[k] = ∑_{i=0}^{k} λ^i/i!
    -- cumSums = [1, 1+λ, 1+λ+λ²/2, ...]
    cumSums :: [FixedPoint]
    cumSums = scanl1 (+) terms

    -- For each k, compute S[k]/p to check if S[k]/p >= e^λ (equivalently: CDF(k) >= p)
    orders :: [FixedPoint]
    orders = map (/ p) cumSums
