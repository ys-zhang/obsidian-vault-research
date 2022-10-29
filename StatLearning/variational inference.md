#machine-learning 
#bayesian-network 
#statistics 


_Variational Inference_ is an **approximate inference algorithm** for _posterior distribution_ of a Bayesian Network.


# Overview

Posterior distribution computing requires integral which makes the problem _intractable_ (no closed analytic form). Exact inference often becomes undecidable.

To solve the problem VI (variational inference) assumes a _parameterised form_ of the _posterior_ and decides the unknown parameter by minimising _KL divergence_ of the result and the true posterior. 

![[Pasted image 20221013212208.png]]


- _Variational distribution_ $p_{\phi}(z|x)$ where
    - $\phi$ is called _variational parameter_;
    - $x$ is _observable_ random variable
    - $z$ is _latent_ variable
- _KL divergence_ $$ KL(p||q) = E_p[\log\frac{p}{q}] = E_p[\log p] - E_p[\log q]$$
# Optimisation Problem

$$
\begin{align}
  q^*(z) = \arg\min_{q\in Q} KL(q || p)
\end{align}
$$

Observe that
$$
\begin{align}
  KL(q||p) &= E_q[\log q] - E_q[\log p(z|x)] \\
  &= E_q[\log q] - E_q[\log\frac{p(z,x)}{p(x)}] \\
  &= E_q[\log q] - E_q[\log p(z,x)] + E_q[\log p(x)] \\
  &= E_q[\log q] - E_q[\log p(z,x)] + \log p(x)
\end{align}
$$
and notice $\log p(x)$ is constant since $x$ are observed.


# ELBO (Evidence Lower Bound)

let 
$$
\begin{align}
  \mathbf{ELBO}(q_\phi) &= E_q[\log p(z,x)] - E_q[\log q] \\
    &= E_q[\log p(x|z)] + E_q[\log p(z)] - E_q[\log q] \\
    &= E_q[\log p(x|z)] - KL[q(z) || p(z)]
\end{align}
$$

**And minimising _KL distance_ is equivalent to maximising _Evidence Lower Bound_**.

>[!NOTE] Balance between Likelihood and Prior
> $$ \mathbf{ELBO}(q_\phi) = E_q[\log p(x|z)] - KL[q(z) || p(z)] $$
> - the first term is _conditional likelihood_
> - the second term is distance to the prior of $z$. 
>
> $\mathbf{ELBO}$ mirrors the usual balance between _likelihood_ and _prior_. 


# Relation with EM algorithm

Notice 
$$ \mathbf{ELBO}(q_\phi) = E_q[\log p(z,x)] - E_q[\log q] $$
and  $E_q[\log p(z, x)]$ is _expected complete likelihood_.

The _EM algorithm_ was designed for finding maximum likelihood estimates in models with latent variables. 
It uses the fact that the _ELBO_ is equal to the log likelihood $\log p(x)$ (i.e., the log evidence) when $q(z) = p(z | x)$.

_EM_ alternates between computing the _expected complete log likelihood_ according to $p(z | x)$ (the E step) and optimising it with respect to the model parameters (the M step).

Unlike variational inference, _EM_ assumes the expectation under $p(z | x)$ is computable and uses it in otherwise difficult parameter estimation problems.

Unlike _EM_, _variational inference_ does not estimate fixed model parameters—it is often used in a Bayesian setting where classical parameters are treated as latent variables. 

Variational inference applies to models where _we cannot compute the exact conditional of the latent variables_.


# Variational Distribution Family

The variational family is not a model of the observed data—indeed, the data $x$ does not appear in $q_\phi(z)$. 

Instead, it is the _ELBO_, and the corresponding _KL_ minimisation problem, that connects the fitted variational density to the data and model.


## Mean-field Variational Family

The idea is to model each _latent parameter_ independently and separately.

>[!NOTE] Assumptions:
> Latent variables are **mutable independent**, i.e.,
> $$ q(z) = \prod_1^m q_j(z_j) $$

The mean-field family is expressive because it can capture any marginal density of the latent variables. However, it cannot capture correlation between them.

## Partial Mean-field
see [[#Partial Mean Field Approximation]]


## Structured mean-field

> It is sometimes the case that a vanilla mean-field distribution is a poor approximation of the posterior, in which case more structured approximations should be used. Deriving the variational update for structured mean-field is harder than vanilla mean-field; however, from a probabilistic program point of view, a structured mean-field approximation is simply a more complex (but still unconditional) variational program that could be derived via _program analysis (or perhaps online via RL state-space estimation)_, with gradients computed as in the mean-field case.



# Inference

## CVAI (Coordinate ascent mean-field variational inference)

Considering all parameters for the latent variable $z$ is know except for $z_j$, i.e. 
$$
\forall i\ne j, \text{ the optimal } \phi_i \text{ is known}
$$
then the optimal $q_j^*(z_j)$ is then proportional to the _exponentiated expected log of complete conditional_:
$$ q_j^*(z_j) \propto \exp \bigg \{ E_{-j} \big [\log p(z_j|\boldsymbol{ z_{-j}, x}) \big ] \bigg \} $$
![[Screen Shot 2022-10-14 at 12.21.48 am.png]]

> [!WARNING]
> The ELBO is (generally) a non-convex objective function. CAVI only guarantees convergence to a **local optimum**, which can be sensitive to initialisation.

>[!NOTE] Exponential Family
> There is a general form of CVAI for models where each complete conditional is in the [[Exponential Family]], these models includes:
> - Bayesian mixture of exponential families
> - factorial mixture models
> - matrix factorisation models
> - [[Linear Models]], [[Probit Regression]]
> - stochastic block models of network
> - hierarchical mixtures of experts
> - mixed-membership models


## stochastic variational inference (SVI)

Model assumption for all components $p_i$ of the following formula
$$
  p_\theta(x, z) = p_\theta(x|z)p_\theta(z) 
$$

1. can **sample** from $p_i$
2. $p_i$ is differentiable with respect to parameter $\theta$

Concepts:

1. _ERP (elementary random procedure)_:  a fixed set of known, atomic random procedures.
2. _Stochastic elements_ are either _ERPs_ or defined as function of other stochastic elements.
3. _Trace_: As the program $f$ runs, it will encounter a sequence of _ERPs_ $x_1,\cdots, x_T$ , and sample values for each. The set of _sampled values_ is called the _trace_ of the program. The probability of the _trace_: $$ p(\boldsymbol x_{1:T}) = \prod_{t=1}^T p_t\big(x_t | \psi_t(\boldsymbol x_{1:t-1}) \big) $$ 

$$
\begin{align}
  \mathbf{ELBO} &= E_q[\log p_\theta(z, x)] - E_q[\log q_\phi(z)] \\
  &= \int_z q_\phi(z)\log p_\theta(z, x) \; \text dz
   - \int_z q_\phi(z) \log q_\phi(z) \text dz
\end{align}
$$
compute derivative against $\phi$ 


### Estimating derivative of ELBO

$$
\begin{align}
  \partial_\phi \mathbf{ELBO} &= \int_z \big (\partial_\phi q_\phi(z) \big ) \cdot \log p_\theta(z, x) \; \text d z - \int_z \big (\partial_\phi q_\phi(z) \big ) \cdot \log q_\phi(z)\;\text d z - E_q\big[ \partial_\phi \log q_\phi(z) \big] \\
  &= \int_z \big (\partial_\phi q_\phi(z) \big ) \cdot \log p_\theta(z, x) \; \text d z - \int_z \big (\partial_\phi q_\phi(z) \big ) \cdot \log q_\phi(z)\;\text d z \\
  &= \int_z \big (\partial_\phi q_\phi(z) \big ) \cdot \log \frac{p_\theta(z, x)}{q_\phi(z)} \; \text d z \\
  &= \int_z \big (\partial_\phi q_\phi(z) \big ) \cdot \log \frac{p_\theta(x|z) p_\theta(z)}{q_\phi(z)} \; \text d z \\
  &= \int_z q_\phi(z) \frac{\big (\partial_\phi q_\phi(z) \big )}{q_\phi(z)} \cdot \log \frac{p_\theta(x|z) p_\theta(z)}{q_\phi(z)} \; \text d z  \\
  &= \int_z q_\phi(z) \partial_\phi \big ( \log q_\phi(z) \big) \cdot \log \frac{p_\theta(x|z) p_\theta(z)}{q_\phi(z)} \; \text d z \\
  &= E_{q_\phi} \bigg[ \partial_\phi \big ( \log q_\phi(z) \big) \cdot \log \frac{p_\theta(x|z) p_\theta(z)}{q_\phi(z)} \bigg] \\
  &= E_{q_\phi} \bigg[ \partial_\phi \big ( \log q_\phi(z) \big) \cdot \big ( \log \frac{p_\theta(x|z) p_\theta(z)}{q_\phi(z)} + K \big ) \bigg] 
\end{align}
$$

Note
- $p_\theta(z)$ and $p_\theta(x|z)$ are specified directly in the program (Bayesian Network DAG).
- $E_{q_\phi}\big[ \partial_\phi (\log q_\phi(z)) \big]$ = 0
- we can use [[Monte Carlo Methods]] to estimate the last expectation, since we can sample from $q_\phi$
- the reason to add $K\in \mathbb R$ is for [[Monte Carlo Methods#Control Variate Method|variance control]], good $K$ can have drastic effects on quality of the estimation.

### Partial Mean Field Approximation

Let 
- $f$ be the _probabilistic program_ which defines the true posterior $p_\theta(z|x)$;
- $g$ be the _variational program_ which defines the variational distribution $q_\phi(z)$

We generate $g$ from $f$ using **partial mean field approximation**.

>[!IDEA]
> The variational program drops dependencies in distribution and only keeps dependency between variables through control flow



Recall the trace probability 
$$ p(\boldsymbol x_{1:T}) = \prod_{t=1}^T p_t\big(x_t | \psi_t(\boldsymbol x_{1:t-1}) \big) $$
we approximate $p_{t+1}\big(x_{t+1}|\psi(\boldsymbol x_{1:t})\big)$ by
$$
  p_{t+1}\big(x_{t+1} | \phi_{t+1}(\boldsymbol x_{1:t}) \big)
$$

Example:
``` python
def prob_prog():
    M = normal()
    if M > 0:
        mu = complete_deterministic(M)
        X = normal(mu)
    else:
        X = uniform()
    return X

def vari_prog():
    M = normal(phi_1)
    if M > 0:
        mu = complete_deterministic(M)
        X = normal(phi_2)
    else:
        X = uniform(phi_3, phi_4)
    return X
```


to compute $\partial_\phi \mathbf{ELBO}$ just need to sample from $q_\phi(z)$ which can be done by tracing back the execution of _variational program_:

1. sample (**we may adopt importance sampling**) $x_t$ from $$q_{\phi_t}(x_t) = p_t\big(x_t|\phi_t(\boldsymbol x_{1:t-1})\big)$$
2. if some $x_t$ is observed we can use the observed value
3. calc $\log q_t(x_t)$
4. compute the target probability $$ \log p\big (x_t|\boldsymbol x_{1:t-1}\big) = \log p_t\big(x_t|\psi_t(\boldsymbol x_{1:t-1})\big)$$
5. calc $\partial_\phi \mathbf{ELBO}$ by sum up $\log q_{\phi_t}(x_t)$, and $\log p\big(x_t | \boldsymbol x_{1:t-1}\big)$, gradient of $q_\phi(\boldsymbol x)$ can be calculated by auto-differentiate/back-propagation 


> [!NOTE] use Neural Network to specify variational parameter
> Given multiple samples of some observable variable $x$. we can 
> specify the _variational distribution family_ by 
> $$ q_\phi(z) = q_{\phi(x)}(z) $$
> where $\phi(x)$ is an NN or linear combination of some known basis function.


### Variance Control

### Rao-Blackwellization 

$$
\frac{\partial \mathbf{ELBO}}{\partial \phi} = E_{q_\phi} \bigg [ \frac{\partial \log q_\phi(z)}{\partial \phi} \cdot \log \frac{p(z, x)}{q_\phi(z)} \bigg]
$$
suppose $q_\phi$ are [[#Mean-field Variational Family]]  
$$
  q_{\boldsymbol \phi}(\boldsymbol z) = 
    \prod_{t=1}^T q_{\phi_t}(z_t)
$$
Let 
$$
\begin{align}
  p(z, x) &= p_j(z, x) \times p_{-j}(z_{-j}, x) \\
  &= p\big(z_j,\mathbf{Mb}(z_j)\big) \times  p\big(z_{-j}|\mathbf{Mb}(z_j)\big)
\end{align}
$$
- $\mathbf{Mb}(z_j)$ is the _Markov blanket_ of $z_j$
- $p_j=p(z_j,\mathbf{Mb}(z_j))$ is the marginal pdf of _Markov blanket_ of $z_j$ and $z_j$
- $p_{-j} = p(z_{-j}|\mathbf{Mb}(z_j))$ is the conditional pdf on _Markov blanket_ of $z_j$


Then we have

$$
\begin{align}
\frac{\partial \mathbf{ELBO}}{\partial \phi_j} &= E_{q_\phi} \bigg [ \frac{\partial \log q_\phi(z)}{\partial \phi_j} \cdot \log \frac{p(z, x)}{q_\phi(z)} \bigg] \\
&= E_{q_1}\dots E_{q_n}\bigg[ \bigg(\partial_j\log q_j(z_j)\bigg )\cdot \bigg(\log p_j(z, x) + \log p_{-j}(z_{-j}, x) - \sum_j\log q_j(z_j)\bigg) \bigg]\\
&= E_{q_j} \bigg[ 
  \bigg(\partial_j\log q_j(z_j)\bigg) \cdot 
  \bigg(E_{q_{-j}}\big[\log p_j(z, x)\big] -\log q_j(z_j) + E_{q_{-j}}\big[\log p_{-j}(z_{-j}, x) - \sum_{i\ne j}\log q_i(z_i)\big] \bigg)
\bigg]
\end{align}
$$

Notice 
$$
E_{q_{-j}}\big[\log p_{-j}(z_{-j}, x) - \sum_{i\ne j}\log q_i(z_i)\big]
$$
is a constant with request to $z_j$ and $q_j$.
thus 
$$
\begin{align}
\frac{\partial \mathbf{ELBO}}{\partial \phi_j} &= E_{q_j} \bigg[ 
  \bigg(\partial_j\log q_j(z_j)\bigg) \cdot 
  \bigg(E_{q_{-j}}\big[\log p_j(z, x)\big] -\log q_j(z_j) +C \bigg) 
\bigg] \\
&= E_{q_j} \bigg[ 
  \bigg(\partial_j\log q_j(z_j)\bigg) \cdot 
  \bigg(E_{q_{-j}}\big[\log p_j(z, x)\big] -\log q_j(z_j) \bigg) 
\bigg]
\end{align}
$$

Let 
- $\boldsymbol z_{(j)}$ be the _Markov blanket_ of variable $z_j$, together with $z_j$. 
- $q_{(j)}$ be the joint pdf of $z_{(j)}$ in $q$

then 
$$ 
\frac{\partial \mathbf{ELBO}}{\partial \phi_j} = 
  E_{q_{(j)}} \bigg[ \bigg(\partial_j\log q_j(z_j|\phi_j)\bigg) \cdot \bigg(\log p_j(\boldsymbol z_{(j)}, \boldsymbol x) - \log q_j(z_j|\phi_j) \bigg)  \bigg]
$$



### Mini-batch (Dealing with Large Sample Size) (Version 1)

Seperate latent variable into _global_ and _local_ latent variables:
  - sample size $N$
  - samples $\boldsymbol x_1, \dots, \boldsymbol x_N$
  - global latent variables $\boldsymbol \beta$ 
  - local latent variables $\boldsymbol z_1, \dots \boldsymbol z_N$

then 
$$ 
p_\theta(\boldsymbol x, \boldsymbol z, \boldsymbol \beta) = p_\theta(\boldsymbol \beta) \prod_{i=1}^N \bigg( p_\theta(\boldsymbol x_i|\boldsymbol z_i, \boldsymbol \beta)\cdot p_\theta(\boldsymbol z_i|\boldsymbol \beta) \bigg )
$$
corresponding mean-field family
$$
  q(\boldsymbol z, \boldsymbol \beta) = q(\boldsymbol \beta | \boldsymbol \lambda) \prod_{i=1}^N \prod_{j=1}^mq(z_{i,j}|\varphi_j)
$$
where:
  - $\lambda$ is the variational parameter for global latent
  - $\varphi_j$ is the variational parameter for local latent param $z_{i, j}, \forall i$.

![[variantion-inference-subsampling.excalidraw | 70%]]

Define the _locally maximised ELBO_ $L(\lambda)$ to be the ELBO when global variational parameter $\lambda$  is held fixed and the local variational parameters $\varphi$ are set to a local optimum $\varphi(\lambda)$.


Under the assumption of model and mean-field family:

$$
\begin{align}
\mathbf{ELBO}(q) &= E_q\big [\log p(\beta, z, x)\big] - E_q \big[\log q \big] \\
&= E_q\big[ \sum_{i=1}^N\log p(x_i,z_i|\beta) + \log p(\beta)\big] - E\big[\log q(\beta) + \sum_{i=1}^N\log q(z_i) \big] \\
&= E_q\big[ \log p(\beta) - \log q(\beta) \big] + \sum_{i=1}^N E_q\big[ \log p(x_i,z_i|\beta) - \log q(z_i) \big]
\end{align}
$$

Thus, we have
$$
\begin{align}
L(\lambda) &= \max_{\varphi_1,\dots,\varphi_N} \mathbf{ELBO}(\lambda, \varphi_1, \dots,\varphi_N) \\
&= E_{q_\lambda}\big[ \log p(\beta) - \log q_\lambda(\beta) \big] + \sum_{i=1}^N \max_{\varphi_i} E_{q_{\lambda,\varphi_i}}\big[ \log p(x_i,z_i|\beta) - \log q_{\lambda,\varphi_i}(z_i) \big]
\end{align}
$$

consider a random variable $I$ uniformly resample $x$ from the sample set $\{x_i\}_{i=1}^N$ , then 

$$
\begin{align}
L(\lambda) &= E_{q_\lambda}\big[ \log p(\beta) - \log q_\lambda(\beta) \big] + \sum_{i=1}^N \max_{\varphi_i} E_{q_{\lambda,\varphi_i}}\big[ \log p(x_i,z_i|\beta) - \log q_{\lambda,\varphi_i}(z_i) \big]  \\
&= E_{q_\lambda}\big[ \log p(\beta) - \log q_\lambda(\beta) \big] + N E_I\bigg[\max_{\varphi_I} E_{q_{\lambda,\varphi_I}}\big[ \log p(x_I,z_I|\beta) - \log q_{\lambda,\varphi_I}(z_I) \big] \bigg]
\end{align}
$$

thus we have the random variable $\mathcal L(x|\lambda)$ is an _unbiased estimator_ of $L(\lambda)$ defined as
$$
\mathcal L(x|\lambda) = E_{q_\lambda}\big[ \log p(\beta) - \log q_\lambda(\beta) \big] + N \max_{\varphi} E_{q_{\lambda,\varphi}}\big[ \log p(x,z|\beta) - \log q_{\lambda,\varphi}(z) \big]
$$
where $x \sim \mathbf{Uniform}(\{x_1,\dots,x_N\})$
then $$ E_x\big[ \mathcal L(x|\lambda) \big] = L(\lambda) $$

>[!ALGORITHM] Mini-batch
> LOOP:
> 1. sample $M \ll N$ observations $\{\tilde x_i\}_{i=1}^M$ from $\{x_i\}_{i=1}^N$
> 2. fix global variational parameter $\boldsymbol \lambda$ and 
> 3. compute optimal local variational parameter $\varphi_i(\lambda)$ for each mini-batch sample $\tilde x_i$
> 4. compute optimal for  global  variational parameter $\lambda$

### Mini-batch (Dealing with Large Sample Size) (Version 2)

recall 

$$
\frac{\partial \mathbf{ELBO}}{\partial \phi_j} = 
  E_{q_{(j)}} \bigg[ \bigg(\partial_j\log q_j(z_j|\phi_j)\bigg) \cdot \bigg(\log p_j(\boldsymbol z_{(j)}, \boldsymbol x) - \log q_j(z_j|\phi_j) \bigg)  \bigg]
$$
where
- $\phi$ includes global variational parameter $\lambda$ and local variational parameter $\varphi$;
- $q_{(j)}$ is the marginal pdf of _Markov blanket_ of $z_j$ and $z_j$ in the target _Bayesian Network_ or distribution $p$.
- $p_j=p(z_j,\mathbf{Mb}(z_j))$ is the marginal pdf of _Markov blanket_ of $z_j$ and $z_j$

Consider sample $i$  and local variational parameter $\varphi_i$ (can be a vector). The _Markov blanket_ of the local latent variable $z_i$ is $(z_i, \beta)$, where $\beta$ is the global latent variable
$$
\frac{\partial \mathbf{ELBO}}{\partial \varphi_i} = 
  E_{q(\beta,z_i)} \bigg[ \bigg(\partial_{\varphi_i}\log q(z_i|\phi_i)\bigg) \cdot \bigg(\log p(z_i, x_i, \beta) - \log q(z_j|\phi_i) \bigg)  \bigg]
$$

and for global variational parameter $\lambda$ and global latent variable $\beta$.
$$
\begin{align}
\frac{\partial \mathbf{ELBO}}{\partial \lambda} &= 
  E_{q(\beta,z)} \bigg[ \bigg(\partial_\lambda\log q(\beta|\lambda)\bigg) \cdot \bigg(\log p(z, x, \beta) - \log q(\beta|\lambda) \bigg)  \bigg] \\
&= E_{q(\beta,z)} \bigg[ \bigg(\partial_\lambda\log q(\beta|\lambda)\bigg) \cdot \bigg(\sum_{i=1}^N\log p(x_i|z_i,\beta) + \sum_{i=1}^N\log p(z_i|\beta) + \log p(\beta) - \log q(\beta|\lambda) \bigg)  \bigg] \\
&= E_{q(\beta,z)} \bigg[ \bigg(\partial_\lambda\log q(\beta|\lambda)\bigg) \cdot \bigg(\log p(\beta) - \log q(\beta|\lambda) \bigg) \bigg] + E_{q(\beta,z)} \bigg[ \bigg(\partial_\lambda\log q(\beta|\lambda)\bigg) \cdot \bigg(\sum_{i=1}^N\log p(x_i|z_i,\beta) + \sum_{i=1}^N\log p(z_i|\beta) \bigg) \bigg]
\end{align}
$$

when $N \gg 1$ we can approximate the second term of RHS by
$$
  \sum_1^N \log p(x_i, z_i|\beta) \approx \frac{N}{M}\sum_{k=1}^M \log p(x_{i_k}, z_{i_k} | \beta)
$$
with $M\ll N$.

