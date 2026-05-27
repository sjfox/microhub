The **CalCopycat** model extends the standard Copycat forecast with empirically calibrated prediction intervals.

It first builds the same pattern-matching forecast as Copycat: recent observed growth rates are compared to spline-smoothed historical anchor trajectories from comparable MMWR weeks, and matched anchors are sampled to generate future trajectories.

It then calibrates uncertainty using leave-one-out (LOO) backtesting on the historical time series. For each held-out forecast origin, CalCopycat:

1. forecasts forward from that historical week using the remaining eligible anchor trajectories,
2. removes any anchor trajectories whose local spline support overlaps the held-out answer window, and
3. records the level residuals between the forecast and what actually happened at each horizon.

These historical residuals are pooled by target group, horizon, and nearby reference week. When generating the live forecast, CalCopycat draws from those residual pools and adds the sampled error to the simulated level trajectories before computing quantiles.

Because the calibration is based on comparable weeks and recent-level regimes, the prediction intervals reflect how uncertain forecasts have actually been for similar situations in the past.
