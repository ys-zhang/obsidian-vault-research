
#book #statistics

# Basic Sampling

## Transform by Inverse CDF

$U \sim \mathrm{Uniform}[0, 1]$,  $F$ is the CDF of distribution $D_F$
then
$$
	F^{-1}(U) \sim D_F
$$
since

$$
\begin{align}
P(F^{-1}(U) \le x) = P(U \le F(x) ) = F(x)
\end{align}
$$

> Problem: $F$ is unknown.
>  The density $f$ may also unknown up to a constant factor.


## Rejection Methods

> PROBLEM:  The target pdf $\pi$ is only known up to a constant factor

Given function $\ell$ and **pdf** $g$ are known and
$$
\begin{align}
	& \ell \propto \pi \\
	\exists M>0 \; s.t. \; &Mg \ge \ell
\end{align}
$$
we want to sample $\pi$.

1. Draw sample from pdf $g$
$$
	X_i \sim D_g
$$
2. Given $X_i$ draw $Y_i | X_i \sim Bin(\frac{\ell(X_i)}{Mg(X_i)})$
3. Accept $X_i$ if $Y_i=1$.

Then $E[X_i|Y_i=1]  \sim D_{\pi}$

Proof:
$$
\begin{align}
	p(X_i = x | Y_i = 1) &= \frac{P( X_i = x, Y_i =1)}{P(Y_i =1)} \\
	&\propto P(Y_i=1|X_i=x) \cdot P(X_i=x)           \\
	&\propto \frac{\ell(z)}{Mg(z)} \cdot g(z)  \\
	&\propto \ell(x)  \\
	&\propto \pi(x)
\end{align}
$$

###  Efficacy

$$
	c =\frac{\ell}{\pi}  = \int \ell(x) \; dx
$$
Efficacy / Accept rate
$$
Accept = \int \frac{\ell(x)}{Mg(x)} g(x)\; dx = \frac{c}{M} 
$$
also see [Effective sample size - Wikipedia](https://en.wikipedia.org/wiki/Effective_sample_size)
###  Applications

- Truncated Gaussian Distribution.  $\phi(x)I_{\{x>c\}}$

## Variation Control

### Stratified Sampling

GIven a **known** pdf 
$$f : \chi \to \mathbb{R}$$
Partition the domain $D$ into sub-region $\chi = \bigcup_{i=1}^k D_i$
Aims to estimate 
$$
	\int_\chi f(x) \; dx = \sum_{i=1}^k \int_{D_i} f(x) \; dx
$$

estimate $\int_{D_i} f(x) \; dx$ by
$$
\hat{\mu}_i = \frac{1}{m_i} \sum_{j=1}^{m_i} f(X^{(i, j)}) 
$$

$$
var(\hat{\mu}) = \sum_{i=1}^k \frac{\sigma_i^2}{m_i}
$$

### Control Variate Method

Aims to estimate $E[X]$.
Given another r.m. $C$, $\mu_C = E[C]$, $var(C)$ and $cov(C, X)$ known.
We can control the variance using the above information to control variance

Let 
$$
X(b) = X - b  (C - \mu_C)
$$

$$
\min_b var(X(b)) = var(X) - 2b \cdot cov(X, C) + b^2 \cdot var(C) 
$$

### Arithmetic Variate Method

####  Observation
For all **monotonic function** $g$,

$$
[g(u_1) - g(u_2)] \cdot [g(1-u_1) - g(1-u_2)] \le 0
$$

#### Proof
Let $X=F^{-1}(U)$ and $X' = F^{-1}(1-U)$ both follows distribution $D_F$.
And $U_1, U_2 \sim Uniform[0,1]$

$$
\begin{align}
0 &\ge E[\{F^{-1}(U_1) - F^{-1}(U_2)\}\{F^{-1}(1-U_1) - F^{-1}(1-U_2)\}] \\
&= E[(X_1 - X_2)(X_1' - X_2')]\\
&= E[X_1X_1'] + E[X_2X_2'] - E[X_1]E[X_2'] - E[X_2]E[X_1']\\
&= 2E[XX'] - 2E[X][X'] \\
&= 2cov[X,X']
\end{align}
$$

$$
\begin{align}
var[\frac{X + X'}{2}] &= \frac{1}{4}E[2X^2 + 2XX'] - E^2[X]\\
&= \frac{var[X]}{2} + cov[XX'] \\
&\le \frac{var[X]}{2}
\end{align}

$$

### Rao-Blackwellization

>**Rule of thumb in Monte Carlo** : 
One should carry out analytical computation as much as possible.
^mc-rule-of-thumb

We can draw from Distribution $\pi$, aims to estimate $I=E_{\pi}[h(X)]$, the [[histogram estimator]] 
$$
\hat{I} = \frac{1}{m} \sum_1^m h(X_i) 
$$

Suppose, in addition, that x can be decomposed into two parts $(x_1, x_2)$ and that the conditional expectation $E[h(x)|x_2]$ can be carried out analytically.

We have the [[mixed estimator]] 
$$
\tilde{I} = \frac{1}{m} \sum_1^m E[h(x)|x_2^{(i)} )]
$$

根据三角不等式， $(Y - E[Y|X]) \perp E[Y|X]$ 
$$
var[Y] = var[E[Y|X]] + E[var[Y|X]]
$$
we have
$$
var[\tilde{I}] \le var[\hat{I}]
$$

>This is highly related to the [[Rao-Blackwell Theorem]].


## Importance Sampling (IS)

The **importance sampling** idea suggests that one should focus on the *region(s)* of "importance" so as to *s*.

Aims to estimate 
$$
\mu = E_{\pi}[h(X)] = \int h(x)\pi(x) \; dx
$$

>**We only knows the distribution $\pi$ up to a multiplicative constant.**

### Algorithm

```python
import pyro

```

1. Draw $x^{(1)}, \dots , x^{(m)}$ from a** trial distribution** $g(·)$.
2. Calculate the **importance weight** 
$$
	w^{(i)} \propto \frac{\pi(x^{(i)})}{g(x^{(i)})}
$$
3. approximate $\mu$
$$
	\hat{\mu} = \frac{\sum w^{(i)} h(x^{(i)})}{\sum w^{(i)}}
$$

>Similar to the rejection method, a successful application of importance sampling in this case requires that the sampling distribution $g$ is reasonably close to $\pi$; in particular, that **$g$ has a longer tail than $\pi$** 


### Efficiency: effective sample size (ESS)

> **Rule of thumb of importance sampling**
> Use the **effective sample size (ESS)** to measure how different the trial distribution is from the target distribution. S

$$
ESS(m) = \frac{m}{1+var_g[w(X)]}
$$

$$
\frac{var_{\pi}[h(x)]}{var_g[h(x)w(x)]} \approx \frac{1}{1+var_g[w(x)]}
$$

### Weighted Sample

A set of ***weighted random samples*** $\{(x^{(j)}, w^{(j)})\}_1^m$ is called _**proper w.r.t **_  distribution $\pi$  if $\forall h \in \mathcal{L}^2$,
$$
	E[h(x^{(j)})\cdot w^{(j)}] = cE_\pi[h(x)], \quad for \; j=1\dots m
$$

where $c$ is the _**normal constant**_ common to all  $m$ samples.

$$
\begin{align}
	\hat{\mu}(h) &= {1 \over W} \sum_{j=1}^m w^{(j)}h(x^{(j)}) \\
	W &= \sum_{j=1}^m w^{(j)}
\end{align}
$$

#### A sufficient and necessary condition

**Equivalently**  suppose joint distribution $(x^{(j)}, w^{(j)}) \sim g(x, w)$
$$
\begin{align}
	E_\pi[h(x)] = { E_g[h(x), w] \over E_g[w] }\\
	 \int h(x)w \; dg(x, w) = \int h(x) \; d\pi(x) \\
\end{align}
$$
or iff.
$$
\begin{align}
	\pi(x) = {E_g[w|x] \over E_g[w]} \cdot g_{\mathrm{marginal}}(x) \\
	\pi \propto E_g[w|x] \cdot g_{\mathrm{marginal}}(x)
\end{align}
$$

####  Proper weighted sample as a generalization of IS

>In the context of importance sampling, the importance weight w is a deterministic function of the corresponding sample $x$ i.e., $E[w|x] \sim \delta(\frac{\pi(x)}{g(x)})$

### Marginalization in IS for variance control

> *Marginalization* : IS $\Leftrightarrow$ *Rao-Blackwellization* : vanilla MC

Let $f, g$ are pdf with $supp(f)\subset supp(g)$. We have

$$
	var_g \{{f(X_1, X_2) \over g(X_1, X_2)}\} \ge var_g\{{f_1(X_1) \over g_1(X_1)}\}
$$

The variances are taken with respect to $g$.

###### Proof
$$
\begin{align}
\frac{f_1(x_1)}{g_1(x_1)} &= \int {f(x_1, x_2) \over g_1(x_1) g_{2|1}(x_2|x_1)}  g_{2|1}(x_2|x_1) \; dx_2 \\
&= E_g[\frac{f(X_1, X_2)}{g(X_1, X_2)}|X_1=x_1]
\end{align}
$$

thus we have

$$
var_g[{f(X_1, X_2) \over g(X_1, X_2)}] = var_g[E_g[\frac{f(X_1, X_2)}{g(X_1, X_2)}|X_1]] + E_g[var_g[\frac{f(X_1, X_2)}{g(X_1, X_2)}|X_1]]
$$

>In the [[ANOVA (Analysis of Variance)]] terminology, is the average "within-group" variation with the group indexed by $X_1$

##    Adaptive importance sampling

The idea is keep updating the trial density $g$ from information gained of
the target distribution $\pi$.

> With weighted Monte Carlo  samples, one can estimate the mean and covariance matrix, denoted as $\mu_1$  and $\Sigma_1$, respectively, of the target distribution.
> Then, a new trial density  can be constructed as $g_1(x) = t_\alpha(x, \mu_1, \Sigma_1)$ (Oh and Berger 1992). 
> This procedure can be iterated until a certain measure of discrepancy between the trial distribution and the target distribution, such as the [[coefficient of variation]] of the importance weights, does not improve any more.

##### Optimal Trial Distributions

Given a Family of trial distributions $g(x;\lambda)$.

The corresponding important weights w.r.t. the target distribution $\pi$:
$$
	w(x;\lambda) \propto \frac{\pi(x)}{g(x;\lambda)}
$$
>  The reader should be cautious in using these adaptive methods since they  
are typically unstable. Perhaps a less greedy but more robust approach is to  
minimize a more robust distance measure between the trial and the target  
densities (e.g., the [[Hellinger distance]] or the [[Kullback-Leibler distance]]).

## Rejection control

> In rejection sampling, instead of have a conservative large $M$ s.t. $\pi \le Mg$, we can  **accept those $x$'s that lie in the region $\{x: \pi(x) > Mg(x)\}$, and adjust the bias by giving these samples appropriate weights.**

##### Algorithm

- Accept probability 
$$
	r_c^{(i)} = \min \{ 1, {w^{(i)} \over c} \} = \min \{1, {\pi(x^{(i)}) \over c\;g(x^{(i)})}\}
$$
-    If the $j$th sample $x^{(j)}$ is accepted, its weight is updated to $w_*^{(j)}$, where
$$
\begin{align}
w_*^{(j)} &= q_c \frac{ w^{(j)} }{ r_c^{(j)} } \\
q_c &= E_g[r_c(x)] = \int r_c(x)g(x) \; dx
\end{align}
$$

>The above RC scheme can be viewed as a technique for adjusting the  
trial density $g$ in light of current importance weights. The new trial density  
$g^*(x)$ resulting from this adjustment is expected to be closer to the target  
function $\pi(x)$.

$$
  g^*(x) = q_c^{-1} \min\{g(x), \frac{\pi(x)}{c}\}
$$

##  Sequential Importance Sampling (SIS)

Target distribution
$$
	\pi(x) = \pi(x_1)\pi(x_2|x_1)\cdots \pi(x_d|x_{1:d-1})
$$
Trail Distribution
$$
	g(x) = g(x_1)g(x_2|x_1)\cdots g(x_d|x_{1:d-1})
$$
weights
$$
w(x) = { \pi(x_1)\pi(x_2|x_1)\cdots \pi(x_d|x_{1:d-1}) \over g(x_1)g(x_2|x_1)\cdots g(x_d|x_{1:d-1})}
$$
or  inductively

$$
w_t(x_{1:t}) = w_{t-1}(x_{1:t-1}) \frac{\pi(x_t|x_{1:t-1})}{g(x_t|x_{1:t-1})}
$$

>  The marginal distribution $\pi(x_{1:t})$ can be used to guide the generation of $x$.
>   - We can stop generating further components of x if the partial weight derived from the sequentially generated  partial sample is too small 
>   - we can take advantage of $\pi(x_t | x_{1:t-1})$  in designing $g(x_t | x_{1:t-1})$

##### Problem

Analytically decompose $\pi$ 

Suppose we can find a sequence of "auxiliary distributions," 
$$\pi_1(x_1), \pi_2(x_{1:2}), \dots , \pi_d(x_{1:d}), $$
so that $\pi_t(x_{1:t})$ is a reasonable approximation to the marginal distribution $\pi(x_{1:t}) \quad for t = 1, 2,\dots, d-1$, but
$$
	\pi_d = \pi
$$

![[SIS-step.png]]

   In the SIS step, we call $u_t$ an ***incremental weight***.
   
   > When we observe that Wt is getting too small, we can choose to reject the sample halfway and restart again. In this way, we avoid wasting  time on generating samples that are doomed to have little effect in the final estimation. However, as an outright rejection incurs bias, the **rejection control technique** described in Section 2.6.2 can be used to correct such bias (Section 2.6.4)
   
   ![[SIS-RC.png]]

