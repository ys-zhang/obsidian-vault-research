
In mathematics, the _Wasserstein distance_ or _Kantorovich–Rubinstein metric_ is a distance function defined between probability distributions on a given _metric space_ $M$.

# Definition

Let $(\mathcal X, \|\cdot\|)$ be a metric space for which every _Borel probability measure_ on $\mathcal X$ is a Radon measure, define the _Wasserstein distance_

$$
\begin{align}
W_p(\mu, \nu) &= \inf_{\gamma \in \Gamma(\mu,\nu)} \big ( \int_{\mathcal X \times \mathcal X} \|x - y\|^p d\gamma(x, y) \big{)}^{1 \over p} \\

&= \inf_{X \sim \mu, Y\sim \nu} (E \|X-Y\|^p )^{1 \over p}

\end{align}
$$


# The problem of optimal transportation (Monge-Kantorovich problem)

Wasserstein distances are _metrics_ between _probability distributions_ that are inspired by the _problem_ of **optimal transportation**.

It origins lie with Monge’s (primarily mathematical) enquiry into how to optimally transport a pile of earth of a given volume into a pit of equal volume but potentially  different shape.

> The problem is to find a _copula_ that couples X and Y together as "tightly" as possible in  an $L_p-$sense, on average; if $p = 2$ then the copula one seeks is the one that _maximizes the correlation (or covariance) between X and Y , i.e., the copula inducing maximal linear dependence_.

###### A simple intuitive interpretation in the discrete case
$$
W_p(\mu, \nu) = \inf_{\gamma \in \Gamma(\mu,\nu)} \big ( \int_{\mathcal X \times \mathcal X} \|x - y\|^p d\gamma(x,y) \big{)}^{1 \over p} 
$$

Given a $\gamma \in \Gamma(\mu,\nu)$, and any pair of locations $(x,y)$, the value of $\gamma(x,y)$ tells us what proportion of $\mu$’s mass at $x$ ought to be transferred to $y$, in order  to reconfigure $\mu$ into $\nu$.

# Properties
Recall the definition
$$
W_p(\mu, \nu) = \inf_{X \sim \mu, Y\sim \nu} (E[\|X-Y\|^p])^{1 \over p}
$$



1.    The space of measures with $p$-th moments finite, the Wasserstein  space $\mathcal W_p(\mathcal M)$, when endowed with the distance $W_p$, is _complete_ and _separable_ if  $\mathcal M$ is so.
2.    $\mathcal M$ can be isomorphically embedded into $\mathcal W_p(\mathcal M)$, since $W_p(\delta_x, \delta_y) = \|x-y\|_p$
3.    Converge in $W_p \iff$ Converge in distribution. 
4.    Application of delta method.
5.    With some regularity assumptions ($\mathcal X=\mathbb{R}^d$, and $\mu$ absolute continues w.r.t. Lebesgue measure), _optimal can be achieved as a deterministic coupling_, i.e.    $γ(A × B) = µ(A\cap T^{-1}(B))$ where $T: \mathcal{ X \to  X}$



![[Wasserstein metric.jpeg]]
