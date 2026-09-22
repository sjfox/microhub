What kind of data are you uploading?

**Counts** — raw case counts, hospitalizations, admissions, or similar. Values are non-negative whole (or near-whole) numbers with no upper bound. This is the default, and matches how MicroHub's models have always worked.

**Proportion (0-1)** — a value that is already a ratio or share, such as test positivity, the fraction of ED visits for a given illness, or bed occupancy. Values must be expressed as a fraction between 0 and 1 (0.42, not 42 or 42%) and are bounded above at 1.

Choosing "Proportion" changes how every model treats your value column: forecasts are produced on a logit (or beta-distribution) scale so they can't be projected below 0 or above 1, and outputs are no longer rounded to whole numbers. On the INFLAenza tab, the "Use population column?" offset is disabled in this mode, since it assumes value is a count to be divided or multiplied by a population -- that doesn't apply to data that's already a proportion. newGBQR and parGBQR have no such checkbox -- they auto-detect a `population` column in the uploaded data instead -- but in Proportion mode they simply ignore any uploaded population column, for the same reason.

FourCAT is not yet adapted for proportion data and should not be used when this is set to "Proportion."
