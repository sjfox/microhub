The number of trajectory samples simulated per held-out season during LOO calibration.

These trajectories are compared to actual outcomes to build the empirical residual distribution used to widen the forecast intervals. More samples produce a richer, smoother residual pool but increase runtime linearly.

**100** is a good default for datasets with up to 10 historical seasons. Reduce to **20–50** for a quicker exploratory run, or increase to **200–300** if you want tighter estimates of the tail residuals and runtime is not a concern.
