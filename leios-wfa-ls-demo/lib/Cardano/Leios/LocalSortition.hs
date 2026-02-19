module Cardano.Leios.LocalSortition where
import Cardano.Crypto.VRF (OutputVRF)
import Cardano.Leios.Types (RelativeStake)
import Cardano.Protocol.TPraos.BHeader (BoundedNatural)

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

Combine this with the fact that we desire that more stake wins you proportionally more seats, we have
that by the desire that 

E[X_totalSeats] = E[\Sum_i λ_i] = n2

giving that we require λ_i = σ_i * n2 (where σ_i is the stake of each party).
We conclude that for a single pool that holds σ stake that λ = σ * n2

Then similarly to the VRF argument for Praos we check that a pool wins k seats if

p < (n2 * σ)^k * e^(-n2*σ) / (k!) <=> (p * k!) / (n2 * σ)^k < e^(-n2*σ)

the latter exponential can be compared in a similar fashion. Moreover, given that

P(X=(k+1)) = ( e^(-λ) * λ^(k+1) ) / (k+1)!
           = ( e^(-λ) * λ^k * λ ) / (k! * (k+1))
           = P(X = k) * (λ / (k+1))

So if we initially calculate P(X=1) and compare 

p < (n2 * σ) * e^(-n2*σ) <=> p / (n2 * σ) < e^(-n2*σ)

Then if that is true (they win at least 1 seat), the next order comparison P(X=2)
can cheaply be checked via

p < (λ / 2) P(X=1) <=> 2p / λ < P(X=1) = (n2 * σ) * e^(-n2*σ) <=> 2p / (λ * n2 * σ) < e^(-n2*σ)

or, given that you define a = p / (n2 * σ) = p / λ, the iterative checks are:

wins 1 seat  :          a       < e^(-λ)         where λ = n2 * σ
wins 2 seats :       2a/λ = b   < e^(-λ)
wins 3 seats :       3b/λ = c   < e^(-λ)
...
wins k seats : k*(prev)/λ       < e^(-λ)

where each check reuses the previous ratio value.

This way, we can compute the Taylor expansion once, and do cheap rational arithmetic recursively
until the comparison does not hold any more.

-}