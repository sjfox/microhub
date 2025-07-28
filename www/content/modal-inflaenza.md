Consider a dataset of observed SARI hospitalizations across three age groups -
pediatric, adult, and overall - collected weekly through Paraguay’s national
surveillance system. Let $Y_{g,t}$ represent the count of hospitalizations for
age group $g$ during week $t$. We model $Y_{g,t}$ as a product of the expected
number of individuals at risk $E_{g,t}$, and a latent relative risk term
$\lambda_{g,t}$, where $E_{g,t}$ is a known offset representing the estimated
population at risk in each age group. This setup allows the model to estimate
the weekly risk of SARI hospitalizations per person in each age group.

Under this structure, we use a Poisson likelihood for the observed counts:

$$
Y_{g,t} \sim Poisson(E_{g,t}\lambda_{g,t})
$$

We model the log-relative risk $log(\lambda_{g,t})$ using a Bayesian
hierarchical framework with a global intercept $\beta_{0}$, a cyclical seasonal
effect $f_{seasonal}(t)$, and a a non-cyclical short-term temporal effect
$f_{short}(t,g)$.

Formally, the latent linear predictor takes the form:

$$
log(\lambda_{g,t}) = \beta_{0}+f_{seasonal}(t)+f_{short}(t,g)
$$

The seasonal effect is modeled as a second-order random walk (RW2) on the
epiweek.  We chose an RW2 process rather than an order 1 (RW1) model to favor a
smoother curve and avoid overfitting the seasonal trend, while the circular
aspect enforces that week 1 and week 52 connect under the estimated RW2
relationship. The non-cyclical short-term temporal effect captures week-to-week
deviations stratified by age group to account for abrupt changes in
hospitalizations patterns. This effect is modeled using an autoregressive
process of order one (AR1) across weeks, grouped by age with exchangeable
correlation across groups. The exchangeable grouping assumes that the temporal
deviations for each age group are a priori equally correlated, allowing the
model to borrow strength across groups when estimating short-term trends.
Introducing an autoregressive hyperparameter governs the degree of temporal
correlation. As the forecast horizon extends further into the future, the AR1
structure gradually pulls predicted values toward zero in expectation, resulting
in a reversion to the seasonal trend. While the influence of short-term
fluctuations diminishes over time, their associated uncertainty continues to
impact the width of prediction intervals,ensuring appropriate forecast
dispersion. Model fitting and forecasting are performed using the INLA framework
via the R-INLA package, leveraging latent Gaussian model and Gaussian Markov
random fields (GMRFs) for computational efficiency
[[1]](https://paperpile.com/c/MFYdpA/iYFMR). Additional technical details are
described in the paper [[2]](https://paperpile.com/c/MFYdpA/36ut).

#### References

1.  Gomez-Rubio V. Bayesian inference with INLA. CRC Press; 2020.
2.  Case BKM, Salcedo MV, Fox SJ. An accurate hierarchical model to forecast
    diverse seasonal infectious diseases. medRxiv. 2025. p. 2025.03.03.25323259.
    [doi:10.1101/2025.03.03.25323259](http://dx.doi.org/10.1101/2025.03.03.25323259)
