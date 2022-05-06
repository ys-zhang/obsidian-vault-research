#statistics #hypothesis-test #timeseries 

> A test for a group of autocorrelations is called a **portmanteau test**, from a French word describing a suitcase or coat rack carrying several items of clothing.


# Box-Pierce Test

$$
  Q = T \sum_{k=1}^\ell r_k^2
  \sim \chi^2_{\ell-K}
$$
where
- The `lag` $\ell$ is the **maximum lag** being considered
- $T$ is the number of observations in this time series
- $r_k$ is the **autocorrelation** for lag $k$
- the `dof` $K$ is the number of parameters in the model.

We suggest using $\ell=10$ for non-seasonal data and $\ell=2m$ for seasonal data, where $m$ is the period of seasonality. However, the test is not good when $ℓ$ is large, so if these values are larger than $T/5$, then use $ℓ=T/5$.

```r
library(feasts)
aug %>% features(.innov, box_pierce, lag = 10, dof = 0)
```


# See also

- [[Hypothesis Tests]]
- [[Ljung-Box Test]]

