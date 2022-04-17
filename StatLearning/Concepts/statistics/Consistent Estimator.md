#statistics 

>[!TLDR]
>Converge to the true parameter in probability


A sequence of estimators

$$
T_n = T_n(X_1, X_2, \dots, X_n)
$$
is a **consistent sequence of estimators** of parameter $\theta$ if 

$$
\lim_{n\to\infty} \Pr_{\theta}(|W_n -\theta|<\varepsilon) = 1 \;\;\forall\varepsilon>0 \; \forall\theta\in\Theta
$$

>[!Theorem]
By [[Chebychev's Inequality]] if 
$$ E_\theta[(T_n-\theta)^2] \to 0 $$
then $T_n$ is consistent
Furthermore, as 
$$E_\theta[(T_n-\theta)^2] = \text{Var}_\theta[T_n] +\text{Bias}_\theta^2[T_n]$$
> We have if 
> 1. $\text{Var}[T_n]\to 0$
> 2. $\text{Bias}_\theta[T_n] \to 0$
> 
> then $T_n$ is consistent.


