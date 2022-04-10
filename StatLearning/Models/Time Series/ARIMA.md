While [[Exponential Smooth Models]] are based on a description of the _trend_ and _seasonality_ in the data, ARIMA models aim to describe the _autocorrelations_ in the data.


- $ARMA(p, q)$ models are stationary, if inversible;
- $ARIMA(p,d,q)$ is non-stationary if $d>0$.


# Differencing (差分)

Idea, differencing may make a non-stationary process to be [[Stationary|stationary]].



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

## Yule-Walker Equation 

###### ACF

$$
\begin{align}
 (\Phi(B)y_t)y_{t-k} &= \varepsilon_ty_{t-k} \\ 
 \Phi(B)\gamma_k &= 0 \\
 \Phi(B)\rho_k &= 0 \\
\end{align}
$$

$$
\begin{bmatrix}
      1, & \rho_1 & \cdots & \rho_{p-1} \\
      \rho_1, & 1 & \cdots & \rho_{p-2} \\
      \vdots, & \vdots & \ddots & \vdots \\
      \rho_{p-1}, & \rho_{p-2} & \cdots & 1 \\
\end{bmatrix}
\times
\begin{bmatrix}
  \phi_1 \\
  \phi_2 \\
  \vdots \\
  \phi_p
\end{bmatrix} 
= 
\begin{bmatrix}
  \rho_1 \\
  \rho_2 \\
  \vdots \\
  \rho_p
\end{bmatrix}
$$
or:
$$
\mathbf P_p \phi = \rho
$$

###### PACF




# MA (moving average model / Linear Filter Model)

This stochastic models are based on an idea originally due to Yule (1927) that

>  An observable time series $y_t$ in which successive values are highly dependent can frequently be regarded as generated from a series of independent ‘‘shocks’’ $\varepsilon_t$.
>  The white noise process $\varepsilon_t$ may be regarded as **a series of shocks that drive the system**.


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




# Linear Model

Linear model:
$$
z_t =\Psi(B) a_t = a_t  + \sum_{j=1}^\infty\psi_ja_{t-j}
$$
the autocovariance:

$$
\gamma_k = \sigma^2_{a}\sum_{j=0}^\infty \theta_j\theta_{j+k}
$$
and the **autocovariance generator function**:

$$
\gamma(X) = \sum_{k=-\infty}^\infty \gamma_kX^k
$$
easy to see:
$$
\gamma(B) = \sigma^2_a\Psi(B)\Psi(B^{-1})
$$





# ARIMA (autoregressive integrated moving average)

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

The word _"integrated"_ (maybe "sum" is more appropriate) comes from the fact that 
$$
\nabla^{-1} = (1-B)^{-1} = 1 + B + B^2 + \cdots + B^n + \cdots
$$



