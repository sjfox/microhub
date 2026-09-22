The **Ensemble** model combines the selected set of models into one shared forecast. Three combination methods are available:

- **Median** (default) — the per-quantile median across the selected models. This is MicroHub's original ensembling behavior.
- **Mean** — the per-quantile mean across the selected models.
- **Linear pool** — treats each model's forecast as a full predictive distribution and mixes those distributions (a "linear opinion pool"), rather than averaging quantile-by-quantile. This is generally considered a more statistically principled way to combine forecasts, since a per-quantile median or mean of several distributions is not itself guaranteed to behave like a coherent single distribution.

Use the Ensemble when you want a more stable result that is less sensitive to the quirks of any single model. Combination is powered by the [hubverse `hubEnsembles`](https://hubverse-org.github.io/hubEnsembles/) package.
