#timeseries

While [[Exponential Smooth Models]] are based on a description of the _trend_ and _seasonality_ in the data, ARIMA models aim to describe the _autocorrelations_ in the data.


- `ARMA(p, q)` models are stationary, if inversible;
- if the roots of $\Phi(B)$ of a ARMA model strictly inside the unit circle, then the time series will show an exponential explode curve. 
- `ARIMA(p,d,q)` is **homogeneous non-stationary** and **nonseasonal** if $d>0$.



# AR (Autoregressive model)

`AR(p)` model
$$
y_t = c + \sum_{i=1}^p \phi_iy_{t-i} + \varepsilon_t
$$
or using the _backshift_ notation:
$$
\Phi(B)y = (1 - \sum_{i=1}^p\phi_iB^i)y = c + \varepsilon_t
$$

For an `AR(1)` model:

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

suppose we do the LSE estimate of $\phi_{k-1,i}$:
$$
\hat y_t = \phi_{k-1,1}y_{t-1} + \phi_{k-1,2}y_{t-2} + \cdots+ \phi_{k-1,k-1}y_{t-k+1}
$$
due stationary $\rho_h=\rho_{-h}$:
$$
\hat y_{t-k} = \phi_{k-1,1}y_{t-k+1} + \phi_{k-1,2}y_{t-k+2} + \cdots+ \phi_{k-1,k-1}y_{t-1}
$$
thus
$$
\phi_{k,k} = \text{corr}(y_t-\hat y_t, y_{t-k}-\hat y_{t-k})
$$
where

$$
\begin{bmatrix}
      1, & \rho_1 & \cdots & \rho_{k-1} \\
      \rho_1, & 1 & \cdots & \rho_{k-2} \\
      \vdots, & \vdots & \ddots & \vdots \\
      \rho_{k-1}, & \rho_{k-2} & \cdots & 1 \\
\end{bmatrix}
\times
\begin{bmatrix}
  \phi_{k,1} \\
  \phi_{k,2} \\
  \vdots \\
  \phi_{k,k}
\end{bmatrix} 
= 
\begin{bmatrix}
  \rho_1 \\
  \rho_2 \\
  \vdots \\
  \rho_k \\
\end{bmatrix}
$$



# MA (moving average model / Linear Filter Model)

This stochastic models are based on an idea originally due to Yule (1927) that

>  An observable time series $y_t$ in which successive values are highly dependent can frequently be regarded as generated from a series of independent ‘‘shocks’’ $\varepsilon_t$.
>  The white noise process $\varepsilon_t$ may be regarded as **a series of shocks that drive the system**.


`MA(q)` model:

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

###### ACF

$$
\rho_k = \begin{cases}
\frac{-\theta_k + \sum_{i=1}^{q-k} \theta_i\theta{k+i}}{1+\sum_{i=1}^q \theta_i^2} \; &k\le q\\
0 \; &k>q
\end{cases}
$$

###### PACF

>[!INFO]
>The finite MA process has an autocorrelation function that is zero beyond a certain point, but since it is equivalent to an infinite AR process, its partial autocorrelation function is infinite in extent and is _dominated by damped exponentials and/or damped sine waves_.



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


# ARMA model

If $d=0$ then the model is an ARMA model.

$$
\Phi(B)z_t=\Theta(B)a_t
$$

> [!NOTE] Strictly in-circle Roots
> 
> ARMA process is stationary if the roots of $\Phi(B)=0$ lie outside the unit circle, and exhibits **explosive nonstationary behaviour** if the roots lie **inside the unit circle**.



# ARIMA (autoregressive integrated moving average)

The word _"integrated"_ (maybe "sum" is more appropriate) comes from the fact that 
$$
\nabla^{-1} = (1-B)^{-1} = 1 + B + B^2 + \cdots + B^n + \cdots
$$

`ARIMA(p,d,q)` model

$$
\Phi(B)\nabla^dy = c + \Theta(B)\varepsilon
$$
where $\nabla = 1-B$.

1. The constant $c$ has an important effect on the long-term forecasts obtained from these models.

      - If $c=0$ and $d=0$, the long-term forecasts will go to zero.
      - If $c=0$ and $d=1$, the long-term forecasts will go to a non-zero constant.
      - If $c=0$ and $d=2$, the long-term forecasts will follow a straight line.
      - If $c≠0$ and $d=0$, the long-term forecasts will go to the mean of the data.
      - If $c≠0$ and $d=1$, the long-term forecasts will follow a straight line.
      - If $c≠0$ and $d=2$, the long-term forecasts will follow a quadratic trend. (This is not recommended, and `fable` will not permit it.)

2. The value of $d$ also has an effect on the prediction intervals — the higher the value of $d$, the more rapidly the prediction intervals increase in size.


## Interpretation of ARIMA Model

> [!INFO] Local Behaviour
> 
> **local behaviour** ($\nabla (z_t - \bar z)$) of a **stationary** time series is **heavily dependent** on the **local level/value** of $z_t$
> (例如 概率回归)

![[Pasted image 20220410191750.png]]


To have an **independent** local behaviour:
1. Model I: $\varphi(B)(z_t+c) = \varphi(B)z_t$, this condition requires
      1. Factorization $\varphi(B) = \phi(B) \cdot \nabla$
      2. $\phi(B)$ is either stationary or have root on the unit circle, since $\phi(B)$ can not be exponentially explode.
2. The model $\phi(B)\nabla z_t = \theta(B)a_t$ is **homogeneous except in level**, in that except for a vertical translation, one part of it looks much the same as another.

> [!Note] homogeneous solution
> 
> for any $C_t$, such that $\phi(B)\nabla^dC_t = 0$,  then $z_t+C_t$ also is a solution. 


## deterministic trend of ARIMA model

$$
\phi(B)\nabla^dz_t = \theta_0 + \theta(B)a_t
$$
let $w_t = \nabla^d z_t$, then
$$
  (1 - \sum\phi_iB^i)w_t = \theta_0 + \theta(B)a_t
$$
take expectation

$$
E[w_t] = {\theta_0 \over 1 - \sum_{i=1}^p \phi_i}
$$
If $\theta_0 \ne 0$ then $z_t$ has a statistic trend at polynomial's $d$th order.

>  A stochastic trend does not require the series to follow the trend pattern seen in the past.



