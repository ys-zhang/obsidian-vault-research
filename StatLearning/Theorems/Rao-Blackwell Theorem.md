>The** Rao–Blackwell theorem** states that if $g(X)$ is any kind of estimator of a parameter $\theta$, then the $E[g(X)|T]$ is typically a better estimator of θ, and is never worse, where $T$ is a [[sufficient statistic]]. 

Let $\delta(X)$ be an estimator of parameter $\theta$,
Let $\delta'(X)=E[\delta(X)|T(X)]$

Then for all **convex Loss function** $L$,
$$
	E[L(\delta')]\le E[L(\delta)]
$$

>Sometimes one can very easily construct a very crude estimator $g(X)$, and then evaluate that conditional expected value to get an estimator that is in various senses optimal.