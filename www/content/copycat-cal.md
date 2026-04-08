The **Copycat (Calibrated)** model extends the standard Copycat model with empirically calibrated prediction intervals. It uses leave-one-out (LOO) backtesting to learn how far off historical forecasts were at each forecast horizon, then injects that observed error into the current forecast trajectories before deriving quantiles.

For each historical season held out in turn, the model simulates trajectories from the same epidemic week as the current forecast using only the remaining seasons as a reference database. The difference between each simulated trajectory and what actually happened — the *residual* — is recorded. These residuals are pooled across all LOO seasons into an empirical distribution per horizon. When generating the real forecast, a random draw from this distribution is added to each trajectory sample before quantiles are computed.

Because the calibration runs from the same point in the season as the current forecast, the residual distribution reflects the specific uncertainty regime of that epidemic phase. Intervals automatically widen at longer horizons because historical residuals are larger further into the future.

Use this alongside the standard Copycat model to assess whether the calibration meaningfully changes the forecast uncertainty for your data.
