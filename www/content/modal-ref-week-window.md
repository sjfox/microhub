The number of epidemiological weeks on either side of the current reference week from which calibration residuals are pooled.

During LOO calibration, the model simulates forecasts from each held-out season starting at the current epidemic week. With a window of **2**, residuals from weeks `current_week − 2` through `current_week + 2` are all included in the pool. This broadens the empirical residual distribution without losing the stratification-by-epidemic-position benefit.

- **0** — use only the exact current reference week (strictest stratification, smallest pool)
- **2** — default; captures nearby seasonal phases and typically provides enough residuals even with few historical seasons
- **5** — maximum; pools broadly across a ~10-week band, useful when historical data is sparse

Larger windows increase the residual pool size (more stable estimates) at the cost of slightly looser stratification by epidemic phase.
