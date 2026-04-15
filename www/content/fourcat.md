The **FourCAT** (Fourier-augmented Calendar Aware Transformer) model is a deep learning model for probabilistic epidemic forecasting. It uses a Transformer encoder with Fourier-based epiweek embeddings to capture seasonal patterns, and outputs calibrated quantile forecasts via a pinball loss objective.

The model is trained across multiple random seeds and forecasts are ensembled by averaging quantile predictions across seeds to improve robustness.

No additional settings are required. FourCAT uses the forecast date, data to drop, and forecast horizon configured in the **Data Upload & Settings** tab.