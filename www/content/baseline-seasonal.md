The **Seasonal Baseline** model uses a Generalized Additive Model (GAM) to capture and forecast seasonal trends in the target indicator. The GAM is trained on historical data using smooth splines over week-of-season. The fitted model extrapolates expected trajectories across forecast horizons, making it suitable for capturing seasonal dynamics and serving as a strong baseline for comparison with more complex models.

When "Data Type" is set to Proportion (0-1), the GAM is fit directly on the raw proportion using a Beta-distribution family (`mgcv::gam(family = betar())`) rather than transforming the value column first, so the model's variance naturally shrinks as the mean approaches 0 or 1.

Use this when you expect strong and consistent seasonal patterns, such as in a well-characterized respiratory virus season.
