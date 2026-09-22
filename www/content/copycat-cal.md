The **CalCopycat** model is Copycat's method of analogues approach
[[1]](https://pubmed.ncbi.nlm.nih.gov/14607808/), run without first building a
season-indexed trajectory database. Where Copycat buckets historical data into
discrete seasons, fits a smoothed growth-rate spline per season, and matches
against that database, CalCopycat matches directly against real historical
dates, so no season boundary ever has to be defined.

Let $C_{w,y,a}$ be the raw hospitalization count for epiweek $w$, year $y$,
and age group $a$. As in Copycat, we work with the observed logarithmic growth
rate,

$$
g_{w,y,a} = \log\!\left(\frac{C_{w,y,a} + 1}{C_{w-1,y,a} + 1}\right),
$$

computed once, continuously, across the entire historical record for each age
group -- there is no per-season restart of this series.

**Matching.** For the current date, we take the last "Recent Weeks to Use"
real growth-rate values. Rather than scoring every date in the historical
record, CalCopycat assumes the data is weekly and compares today against the
same calendar position 1 year back, 2 years back, 3 years back, and so on --
exact 52-week multiples. Each of those anchors is widened into a ring of
neighboring weeks by the "Respiratory Week Range" setting (default 2 weeks,
same meaning as Copycat's): any historical week within that many weeks of a
yearly anchor becomes a candidate. This is plain calendar-week arithmetic,
not epiweek, so there's no 52-vs-53-week-year boundary handling to get
wrong, and because only a small ring of weeks around each yearly anchor is
ever scored -- not the entire historical record -- matching stays fast even
with many years of history.

Each candidate in the ring is then scored on *trend similarity* -- the mean
squared difference between the candidate's own trailing growth-rate window
and the current one, using only the real, observed values of both (nothing
is smoothed or estimated).

**Eligibility.** A candidate is only added to the matching pool if it has a
*full*, real, gap-free trailing window the same length as the current one --
no shorter, partial-overlap window is ever accepted as a stand-in. Comparing
a 4-week estimate to a 12-week estimate on the same raw-error footing would
be comparing two quantities with very different sampling noise, so a
candidate whose own recorded history doesn't reach back far enough (or whose
window straddles a data gap) is simply left out. The same standard applies
looking forward: a candidate's real data must actually reach every week the
forecast horizon needs, and its own matching window must be far enough from
today's that it cannot simply be matching against itself. CalCopycat never
extends a short run of historical data with an estimated or interpolated
growth rate to make it reach further than it really does, in either
direction.

**Resampling.** Each eligible candidate's matching error (mean squared
difference between its trailing growth-rate window and the current one) is
converted to a resampling score of $\exp(-\text{error} / h)$, where $h$ is
set by the single best (lowest-error) match actually found this time --
candidates close to that best match get resampled often, candidates far from
it get resampled rarely, regardless of how many candidates happen to be in
the pool. $h$ is never allowed to fall below a data-derived "noise floor" (twice
the variance of the current trend window, i.e. the level of disagreement
you'd expect from two unrelated series by pure chance): this keeps a single
suspiciously-perfect match from collapsing the reference scale down to
itself and swamping every other candidate, while still rewarding it for
being genuinely better than chance. We then randomly sample 1,000 candidate
weeks with replacement using these scores as weights.

**Simulation.** For each sampled candidate, we take the *real, observed*
growth-rate path for the weeks immediately following it -- not a fitted or
smoothed curve -- and apply it to the current value to produce one simulated
future trajectory. To keep a thin candidate pool (even a single historical
analog) from collapsing into a handful of rigid, repeated trajectories, each
week of the growth path also gets its own random perturbation:
$\text{Normal}(0, \text{sd} = \sqrt{\max(\text{that candidate's own
error}, \text{the noise floor}) / n})$, where $n$ is the matching window
length ("Recent Weeks to Use"). Dividing by $n$ turns the raw, single-week
noise level into the standard error of the *window's average* growth rate --
our uncertainty about the underlying trend, which is what this perturbation
should reflect, rather than the raw week-to-week noisiness of individual
observations (already captured by which real historical week got matched).
A candidate that matched
poorly gets shaken around more, one that matched almost perfectly gets
barely perturbed, and drawing a fresh perturbation at every horizon step
(rather than reusing one shift for the whole path) makes the resulting
uncertainty widen the further out the forecast goes the way a random walk
should -- spread grows with the square root of the horizon -- without
needing a fitted curve to hang it on.

There is no separate observation-noise layer added on top of the simulated
trajectories -- the growth-rate perturbation above already reflects the
data's own volatility, so an additional Poisson- or Beta-noise step (as used
in Copycat) turned out to be redundant at best and prone to badly distorting
small proportions at worst. Instead, the one thing that layer used to also
do -- turning a continuous simulated value into a realistic one -- is handled
directly: Counts forecasts are rounded to the nearest non-negative integer
(a real hospitalization count is a whole number), while Proportion forecasts
are left as continuous values, clipped to [0, 1].

The resulting 1,000 sample trajectories are summarized at each horizon
according to the specified quantile levels.

#### References

1.  Viboud C, Boëlle P-Y, Carrat F, Valleron A-J, Flahault A. Prediction of the
    spread of influenza epidemics by the method of analogues. Am J Epidemiol.
    2003;158: 996–1006.
