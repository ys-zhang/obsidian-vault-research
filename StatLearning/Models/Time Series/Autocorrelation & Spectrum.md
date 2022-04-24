#timeseries  #statistics

# Autocorrelation

###### ACF (autocorrelation function)

$$
\text{ACF}(k) = \text{corr}(Y_t, Y_{t-k})
$$

###### PACF (partial autocorrelation function)

$$
\text{PACF}(k) = \text{corr}(Y_t, Y_{t-k} | Y_{t-1}, \dots, Y_{t-k+1})
$$

Assume weak stationary of $y_t$.

Let 
- $\mathbf \Gamma_n$ be the _autocovariance_ matrix
- $\mathbf P_n$ be the _autocorrelation_ matrix

$$
\begin{align}
  \mathbf \Gamma_n &= \;\;\;\;
    \begin{bmatrix}
      \gamma_0, & \gamma_1 & \cdots & \gamma_{n-1} \\
      \gamma_1, & \gamma_0 & \cdots & \gamma_{n-2} \\
      \vdots, & \vdots & \ddots & \vdots \\
      \gamma_{n-1}, & \gamma_{n-2} & \cdots & \gamma_{0} \\
    \end{bmatrix} \\ \\
   &= \gamma_0
     \begin{bmatrix}
      1, & \rho_1 & \cdots & \rho_{n-1} \\
      \rho_1, & 1 & \cdots & \rho_{n-2} \\
      \vdots, & \vdots & \ddots & \vdots \\
      \rho_{n-1}, & \rho_{n-2} & \cdots & 1 \\
    \end{bmatrix} \\ \\
   &= \gamma_0 \; \mathbf P_n
\end{align}
$$

Suppose a _Linear Filter_ 
$$
  L = \psi_0 + \psi_1B + \cdots + \psi_nB^{n} + \cdots
$$
then
$$
\begin{align}
\text{Var}[L(z_t)] &= \sum_{i=1}^\infty\sum_{j=1}^\infty \psi_i\psi_j\gamma_{|i-j|} \\
\text{Cov}[L(z_t), L(z_{t-k})] &= \sum_{i=1}^\infty\sum_{j=1}^\infty \psi_i\psi_j\gamma_{|i-j+k|} 
\end{align}
$$
If $L$ is _stationary_, i.e., $\sum |\psi_i| < \infty$ then the above converges.

## Estimate Mean

Since the process is _stationary_, i.e., $\forall t, s; \; E[z_t] = E[z_s] = \mu$, the sample mean $\bar z = \frac{\sum_{t=1}^N z_t}{N}$ is an unbiased estimator of $\mu$.

$$
\begin{align}
\text{Var}[\bar z] &= {1 \over N^2} \sum_{t=1}^N \sum_{s=1}^N \gamma_{|t-s|} \\
&= \frac{\gamma_0}{N} [ 1 +2\sum_{k=1}^{N-1}(1-{k\over N})\rho_k ] \\
& \to  \frac{\gamma_0}{N} ( 1 +2\sum_{k=1}^{\infty}\rho_k )
\end{align}
$$
**Autocorrelation can affect the precision of the sample mean estimator.**

## Estimate Autocorrelation

$$
\begin{align}
\hat \gamma_k &= c_k = {1 \over N} \sum_{t=1}^{N-k} (z_t-\bar z)(z_{t+k}-\bar z) \\
\hat \rho_k &= r_k = {\hat \gamma_k \over \hat \gamma_0} = {c_k \over c_0}
\end{align}
$$
###### Bartlett approximation

$$
\text{Var}[\hat \rho_k] \simeq {1\over N} \sum_{v=-\infty}^\infty (\rho_v^2 + \rho_{v+k}\rho_{v-k}-4\rho_k\rho_v\rho_{v-k} + 2\rho_k^2\rho_v^2)
$$
If $\forall v>q,\; \rho_v = 0$  then 
$$
\begin{align}
\forall k>q &: \\ \\
  &\text{Var}[\hat \rho_k] \simeq {1\over N}(1+2\sum_{v=1}^q\rho_v^2)  \\
  &\text{Cov}[\hat \rho_k, \hat \rho_{k+s}] \simeq {1\over N}\sum_{v=-q}^q\rho_v\rho_{v+s}
\end{align}
$$

>[!WARNING]
>This result shows that care is required in the interpretation of individual autocorrelations because **large covariances can exist between neighbouring values**.


# Partial Autocorrelation

$$
\alpha(k) = \text{corr}(z_{t+k}-P_{t,k}(z_{t+k}), z_{t}-P_{t,k}(z_{t}))
$$
where  is the surjective operator of **orthogonal projection** of $x$ onto the linear subspace of Hilbert space spanned by $(z_{t+1},\dots,z_{t+k-1})$.


# Spectrum

Another way of analysing a time series is based on the **assumption** that it is _made up of sine and cosine_ waves with different frequencies.


###### Periodogram

$N=2q+1$:
$$
z_t = \alpha_0 + \sum_{i=1}^q[\alpha_i \cos(\frac{2\pi i}{N}t) + \beta_i \sin(\frac{2\pi i}{N}t)] + e_t
$$

$\sin(\frac{2\pi i}{N} t)$ , $\sin(\frac{2\pi i}{N} t)$ is an _orthogonal basis_.

###### Intensity (Sample Spectrum)
the **intensity** at frequency $f$  is defined as 

$$
I(f) = {2\over N}(\alpha_f^2 + \beta^2_f)
$$
>[!NOTE] 
>the sample spectrum is the Fourier cosine transform of the estimate of the autocovariance function

If $0\le f\le{1\over 2}$
$$
\begin{align}
I(f) &= 2[\hat \gamma_0 + 2\sum_{k=1}^{N-1}\hat\gamma_k\cos(2\pi f k)] \\
p(f) &= 2[\gamma_0 + 2\sum_{k=1}^{\infty}\gamma_k\cos(2\pi f k)]\\
g(f) &= {p(f)\over \gamma_0} = 2[1 + 2\sum_{k=1}^{\infty}\rho_k\cos(2\pi f k)]\\
\gamma_k &= \int_0^{1/2} cos(2\pi fk) p(f)\; df \\
1&=\int_0^{1/2} g(f)\;df 
\end{align}
$$
where
- $p(f)$ is called the **power spectrum**;
- $g(f)$ is called the **spectral density function**.

>[!NOTE]
> Stationary time series of the kind described with $L$ are characterized by _random changes of frequency, amplitude, and phase_. 
> 
> For this type of series, the sample spectrum $I(f)$ **fluctuates wildly** and is not capable of any meaningful interpretation.




