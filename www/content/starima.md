The **STArima** model combines seasonal decomposition with autoregressive time-series forecasting.

For each target group, the model:

1. Applies a Box-Cox variance-stabilizing transformation, with the lambda chosen using a Guerrero-style seasonal stability criterion.
2. Uses STL decomposition with a periodic 52-week seasonal component when at least 104 weeks are available.
3. Fits an automatically selected ARIMA model to the seasonally adjusted series.
4. Repeats the most recent seasonal cycle into the forecast horizon and recombines it with the ARIMA forecast.
5. Uses bootstrapped residual simulations to produce empirical quantiles, with forecasts floored at zero.

When fewer than 104 observations are available for a target group, STArima skips the STL decomposition and fits the Box-Cox ARIMA model directly:

`ARIMA(box_cox(value, lambda))`

When at least 104 observations are available, STArima uses this model specification:

`decomposition_model(STL(box_cox(value, lambda) ~ season(period = 52)), ARIMA(season_adjust))`

When "Data Type" is set to Proportion (0-1), no Box-Cox transform is applied. Instead, the series is transformed with a logit before the STL/ARIMA decomposition described above, and the resulting forecasts (and bootstrapped quantiles) are inverse-logit transformed back to the proportion scale, which bounds them to (0, 1) rather than only flooring at zero.
