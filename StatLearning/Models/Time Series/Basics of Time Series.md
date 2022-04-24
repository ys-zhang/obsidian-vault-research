#timeseries  #statistics

[Forecasting: Principles and Practice (3rd ed) (otexts.com)](https://otexts.com/fpp3/)
[Econ 584: Time Series Econometrics (washington.edu)](https://faculty.washington.edu/ezivot/econ584/econ584.htm)



# R packages
[seasonal: R interface to X-13ARIMA-SEATS](http://www.seasonal.website/seasonal.html)

- `install.packages(seasonal)`: season decomposition
- `install.packages(feasts)`:  features of times series, include white noise, stationary, etc.



# Decomposition

Two modes of decomposition
1. additive model: $y_t = S_t + T_t + R_t$
2. multiplicative model: $y_t = S_t \times T_t \times R_t$

The additive method is preferred when the seasonal variations are roughly constant through the series, while the multiplicative method is preferred when the seasonal variations are changing proportional to the level of the series

Four Algorithms:
1. Classical (Moving Average)
2. X-11
3. SEATS
4. STL (Loess) 


> [!NOTE]  Reference
> 
> Dagum, E. B., & Bianconcini, S. (2016). _Seasonal adjustment methods and real time trend-cycle estimation. Springer_. [[Amazon](http://buy.geni.us/Proxy.ashx?TSID=140570&GR_URL=http%3A%2F%2Fwww.amazon.com%2Fdp%2F3319318209)]


## Features

```r
tourism |> features(Trips, feat_stl)
```

### Strength of Trend

$$
F_T = \max \big (0, 1 - \frac{Var[R_t]}{Var[T_t+R_t]} \big)
$$

### Strength of Seasonality

$$
F_S = \max \big (0, 1 - \frac{Var[R_t]}{Var[S_t+R_t]} \big)
$$



## Methods

### Classical Decomposition

1. trend is estimated by moving average, the window of MA is set to season period
2. seasonal part is estimated by average the detrended values for that season, e.g., average all January value as the value of January.

_Problems of Classical Method_:
1. The estimate of the trend-cycle is unavailable for the first few and last few observations.
2. The trend-cycle estimate tends to _over-smooth rapid rises and falls_ in the data.
3. Classical decomposition methods assume that the _seasonal component repeats_ from year to year.
4. Occasionally, the values of the time series in a small number of periods may be particularly unusual.


### X-11

1. trend-cycle estimates are available for all observations including the end points
2. the seasonal component is allowed to vary slowly over time.
3. handles trading day variation, holiday effects and the effects of known predictors.

```r
us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components() |>
  autoplot() +
    labs(title = "Decomposition of total US retail employment using X-11.")
```


### SEATS (Seasonal Extraction in ARIMA Time Series)

```r
us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components() |>
  autoplot() +
    labs(title = "Decomposition of total US retail employment using SEATS")
```


### STL (Seasonal and Trend decomposition using Loess)

1. Unlike SEATS and X-11, STL will _handle any type of seasonality_, not only monthly and quarterly data.
2. The seasonal component is allowed to _change over time_, and the rate of change can be controlled by the user.
3. The _smoothness_ of the trend-cycle can also be controlled by the user.
4. It can be _robust_ to outliers (i.e., the user can specify a robust decomposition)


```r
us_retail_employment |>
  model(
    STL(Employed ~ trend(window = 7) +
                   season(window = "periodic"),
    robust = TRUE)) |>
  components() |>
  autoplot()
```

The two main parameters to be chosen when using STL are the 
- trend-cycle window `trend(window = ?)`  is the number of consecutive observations to be used when estimating the trend-cycle
- and the seasonal window `season(window = ?)` is the number of consecutive years to be used in estimating each value in the seasonal component. Setting the seasonal window to be infinite is equivalent to forcing the seasonal component to be periodic `season(window='periodic')`
These control how rapidly the trend-cycle and seasonal components can change. 
- Smaller values allow for more rapid changes. 
- Both trend and seasonal windows should be **odd numbers**.


# Judgemental Forecast

1. lack of historical data
2. data are incomplete, or only become available after some delay. (**nowcasting**)



Three settings of _judgemental forecast_
1. there are no available data, so that statistical methods are not applicable and judgmental forecasting is the only feasible approach; 
2. data are available, statistical forecasts are generated, and these are then adjusted using judgment; 
3. data are available and statistical and judgmental forecasts are generated independently and then combined.

## Properties

A _judgmental approach_ can be quick to _adjust to changes, information or events_. The accuracy of judgmental forecasting improves when the forecaster has 
 1. important domain knowledge
 2. more timely, up-to-date information.

- Judgmental forecasting is **subjective**, and may suffer from **bias**.
- Judgmental forecasts can be **inconsistent**.
- commonly seen in judgmental forecasting is the effect of **anchoring**.

## The Delphi Method

The method relies on the key **assumption** that _forecasts from a group are generally more accurate than those from individuals_.

The Delphi method generally involves the following stages:

1. A panel of experts is assembled.
2. Forecasting tasks/challenges are set and distributed to the experts.
3. Experts return initial forecasts and justifications. These are compiled and summarised in order to provide feedback.
4. Feedback is provided to the experts, who now review their forecasts in light of the feedback. This step may be iterated until a satisfactory level of consensus is reached.
5. Final forecasts are constructed by aggregating the experts’ forecasts.

## Scenario forecasting

The aim of this approach is to generate forecasts based on plausible _scenarios_.

Önkal, D., Sayım, K. Z., & Gönül, M. S. (2013). Scenarios as channels of forecast advice. _Technological Forecasting and Social Change_, _80_(4), 772–788. [DOI](https://doi.org/10.1016/j.techfore.2012.08.015)
Ord, J. K., Fildes, R., & Kourentzes, N. (2017). _Principles of business forecasting_ (2nd ed.). Wessex Press Publishing Co. [Amazon](http://buy.geni.us/Proxy.ashx?TSID=140570&GR_URL=http%3A%2F%2Fwww.amazon.com%2Fdp%2F0999064916)

