The **Regular Baseline** model fits a flatline-style model that learns from all historical step sizes (week-to-week differences) in the data. A square-root transformation is applied to stabilize variance and improve symmetry. The model symmetrizes past differences and generates forecasts by sampling from this distribution, ensuring the median prediction at all horizons equals the most recent observed value.

Use this as a general-purpose baseline when you want to leverage long-term historical behavior for stable, conservative forecasts.

When "Data Type" is set to Proportion (0-1), a logit transform is used in place of the square-root transform, and simulated forecasts are bounded to (0, 1) rather than only floored at zero.