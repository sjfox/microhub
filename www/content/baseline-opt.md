The **Opt Baseline** model is a variant of the flatline-style approach that learns only from the past 8 weeks of data, rather than the entire history. Square-root transformation and symmetrization are used for better calibration and responsiveness.

This model is often harder to beat than the regular baseline, especially in dynamic or changing conditions, and was empirically selected as the best-performing flatline variant across multiple settings, hence the name “opt” (optimal).

When "Data Type" is set to Proportion (0-1), a logit transform is used in place of the square-root transform, and simulated forecasts are bounded to (0, 1) rather than only floored at zero.