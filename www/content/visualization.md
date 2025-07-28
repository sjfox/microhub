The visualization shows the model-specific forecasts for each target group.

Black dots indicate the recent season’s data that was used by the forecast
model, blue dots indicate the recent data (if any) that was excluded from the
forecast model, and the red dots show the data from the previous year.

For the forecasts, the black line indicates the expected path of the epidemic,
with the 50% and 95% prediction intervals indicated by the dark and light grey
shaded regions respectively. We use these plots during our weekly forecasts to
identify potential issues or parameters that may need to be tuned.

For example, if the most recent data point is significantly lower than the other
recent trends and is out of touch with expected seasonality, it provides
evidence that we may want to drop that data point from being included in the
forecast model because it will likely have significant backfill in future weeks.
