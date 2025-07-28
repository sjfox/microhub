Copycat is based on a method of analogues approach in which recent epidemic
dynamics are projected forward by borrowing and adapting growth patterns
observed from historical data [[1]](https://paperpile.com/c/MFYdpA/hBSI). The
model is built on trajectories of the growth rate rather than the raw counts, so
that time-series that are on different scales can still inform predictions for
one another (i.e. it assumes that the shape of epidemics can be the same even if
the scale differs). We (1) create a growth rate trajectory database from
historical data, (2) summarize recent growth rate trends, (3) match the recent
data against the trajectory database, and (4) sample possible forecast
trajectories from the closest matches to generate plausible future trajectories
of the epidemic. The result is an empirical predictive distribution that
naturally reflects both the variability of past patterns and the uncertainty
inherent in current trends.

Let $C_{w,y,a}$ be the raw hospitalization count for epiweek, $w$, for a given
year, $y$, and for age group, $a$. We compute the observed logarithmic growth
rate as:

$$
g_{w,y,a} = log(\frac{C_{w,y,a} + 1}{C_{w-1,y,a} + 1 }) 
$$

For $w = 2,...,52$ for all historical years and for $w = 2,...,W - k$, for the
current season being forecasted where $W$ is the most recent week of data and
$k$ indicates how many recent weeks to drop. $k$ is tuned weekly based on
observed reporting rates and inconsistencies such as if there appears to be an
anomalous drop in case counts that will later be backfilled. To create the
historical trajectory database we summarize the dynamics for all age groups
(Adult, Pediatric, and Overall) and years from $y = 1,...,Y - 1$, where $Y$ is
the current year using splines. For each combination of age and year from the
historical data, we fit a spline to $g_{w,y,a}$ with a knot every four weeks,
and we record the mean ($\mu_{y,a}(w)$) and standard deviation
($\sigma_{y,a}(w)$) from the model as a function of the epiweek, $w$.

We match the most recent $n$ weeks of the recent season against the trajectory
database by epiweek $w$, which assumes that seasonal trends are likely to be
consistent across years. The default setting for copycat is to match against all
data from the current year ($n = 100$), but sometimes it is useful to match
against only recent weeks of data (typically this occurs towards the end of the
season). Lowering $n$ below the value of $W - k$ achieves this effect. We
calculate the match of the recent seasonal data against every spline as:

$$
E_{a,y,s}=\frac{1}{n}\sum_{i=1}^{n}(gW-k-n+i,Y,a-\mu_{y,a}(W-k-n+i+s))^4
$$

With $s\in\left\{-R,-R+1,...,R-1,R\right\}$, where $R$ is the respiratory week
range, which allows for flexibility in the matching by epiweek as some seasons
may look similar to historical ones when shifted by multiple weeks. We set
$R = 2$ by default, though it can be tuned as needed. We enforce a minimum error
value of 0.02 to prevent perfect fits and we choose only the top 20 closest
matches from the spline trajectory database. We then randomly sample 1,000
trajectories with replacement from those matches with weight equal to the
inverse estimated error.

To produce forecasts we extend the observed growth rate time series by $h$ time
steps where $h$ is equal to the desired forecast horizon plus the number of
recent weeks dropped. For each of the 1,000 selected splines, we retrieve a
forecasted logarithmic growth rate trajectory, $\hat{g}_{h}$ according to:

<div style="text-align:center;">
$$
\hat{g}_{h} \sim N (\mu_{y,a}(W-k+h+s), \sigma_{y,a}(W-k+h+s)),
$$
</div>

Where $s$ is the specific shift used in the matched spline. To convert these
growth rates to forecasts for counts for each trajectory we:

1.  Exponentiate $\hat{g}_{h}$ 

2.  Calculate the cumulative growth rate for trajectory $l$, $\hat{G}_{l,h}$,
    from week $W-k$ to $W-k+h$ 

3.  Produce mean forecasts for each trajectory
    $\lambda_{l,h}=C_{W-k,y,a}\cdot \hat{G}_{l,h}$

4.  Draw a sample from a Poisson distribution and subtract one to it, which adds
    additional anticipated uncertainty from the reporting process, ensures that
    all values are integers, and corrects for the original addition of 1 to all
    values: $\hat{C}_{l,h}\sim Pois(\lambda_{l,h}-1)$

We ensure all values are above zero by taking $argmax(\hat{C}_{l,h}, 0)$. These
steps provide 1,000 sample trajectories for the case counts for each age group
and horizon, which we summarize according to the specified quantiles of the
Paraguay Forecast Hub.

#### References

1.  Viboud C, Boëlle P-Y, Carrat F, Valleron A-J, Flahault A. Prediction of the
    spread of influenza epidemics by the method of analogues. Am J Epidemiol.
    2003;158: 996–1006.
