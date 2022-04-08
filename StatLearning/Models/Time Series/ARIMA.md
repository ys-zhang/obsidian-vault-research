While [[Exponential Smooth Models]] are based on a description of the _trend_ and _seasonality_ in the data, ARIMA models aim to describe the _autocorrelations_ in the data.


# Differencing (差分)

Idea, differencing may make a non-stationary process to be [[Stationary|stationary]].

# correlation

## ACF (autocorrelation function)

$$
ACF(k) = \text{corr}(Y_t, Y_{t-k})
$$

## PACF (partial autocorrelation function)

$$
PACF(k) = \text{corr}(Y_t, Y_{t-k} | Y_{t-1}, \dots, Y_{t-k+1})
$$


# AR (Autoregressive model)



$AR(p)$ model
$$
y_t = c + \sum_{i=1}^p \phi_iy_{t-i} + \varepsilon_t
$$
or using the _backshift_ notation:
$$
\Phi(B)y = (1 - \sum_{i=1}^p\phi_iB^i)y = c + \varepsilon_t
$$



For an AR(1) model:

-   when $\phi_1=0$ and $c=0$, $y_t$ is equivalent to _white noise_;
-   when $\phi_1=1$ and $c=0$, $y_t$ is equivalent to a _random walk_;
-   when $\phi_1=1$ and $c\ne 0$, $y_t$ is equivalent to a _random walk with drift_;
-   when $\phi_1<0$, $y_t$ tends to _oscillate_ around the mean.

There are some other constrains on $\phi$ required to conform to the stationary constraint, i.e., the root of $\Phi(z) \in \mathbb C$ is in the unit circle.


# MA (moving average model / Linear Filter Model)

This stochastic models are based on an idea originally due to Yule (1927) that

>  An observable time series $y_t$ in which successive values are highly dependent can frequently be regarded as generated from a series of independent ‘‘shocks’’ $\varepsilon_t$.


$MA(q)$ model:

$$
y_t = c + \varepsilon_t + \sum_{i=1}^q\theta_i\varepsilon_{t-i} 
$$
or
$$
y = c + \Theta(B)\varepsilon
$$

The linear operator $\Theta(B)$ is dubbed as the **transfer function** of the filter. If the sequence $\{\theta_t\}$ is _absolutely summable_, then the filter $\Theta(B)$ is said to be **stable** and the process $y_t$ is **stationary**.


- It is possible to write any stationary $AR(p)$ model as an $MA(\infty)$ model.
- The reverse result holds if we impose some constraints on the _MA parameters_. Then the MA model is called **invertible**. That is, we can write any invertible $MA(q)$ process as an $AR(\infty)$ process.


# ARIMA

$ARIMA(p,d,q)$ model

$$
\Phi(B)(1-B)^dy = c + \Theta(B)\varepsilon
$$

The constant cc has an important effect on the long-term forecasts obtained from these models.

-   If $c=0$ and $d=0$, the long-term forecasts will go to zero.
-   If $c=0$ and $d=1$, the long-term forecasts will go to a non-zero constant.
-   If $c=0$ and $d=2$, the long-term forecasts will follow a straight line.
-   If $c≠0$ and $d=0$, the long-term forecasts will go to the mean of the data.
-   If $c≠0$ and $d=1$, the long-term forecasts will follow a straight line.
-   If $c≠0$ and $d=2$, the long-term forecasts will follow a quadratic trend. (This is not recommended, and `fable` will not permit it.)

The value of $d$ also has an effect on the prediction intervals — the higher the value of $d$, the more rapidly the prediction intervals increase in size.


