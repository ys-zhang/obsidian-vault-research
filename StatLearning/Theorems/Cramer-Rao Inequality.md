#statistics 

>[!TLDR]
>Any estimator of some distribution $f_\theta$, the variance of the estimator is bounded by the _inverse_ of **Expected Fisher Information** $I(\theta)$


Let $\mathbf X = (X_1, \dots, X_n)$ and $\{X_i\}$ be samples  satisfies,
1. _pdf_: $\mathbf X \sim f(\mathbf x|\theta)$
2. $$ \frac{d}{d\theta}E_\theta[W(\mathbf X)] = \int_\chi\frac{\partial}{\partial\theta}[W(\mathbf x)f(\mathbf x|\theta)]\; d\mathbf x $$
3. _Finite variance_ $\text{Var}_\theta[W(\mathbf X)] <\infty$

Then we have the **Cramer-Row Lower Bound** of estimator $W$'s variance

$$
\text{Var}_\theta[W(\mathbf X)] \ge {(\frac{d}{d\theta}E_\theta[W(\mathbf X)])^2 \over E_\theta\big[ (\frac{\partial}{\partial\theta}\ln f(\mathbf X|\theta))^2\big]}
$$

if $X_i$ are _i.i.d._ then

$$
\text{Var}_\theta[W(\mathbf X)] \ge {(\frac{d}{d\theta}E_\theta[W(\mathbf X)])^2 \over n E_\theta\big[ (\frac{\partial}{\partial\theta}\ln f(X|\theta))^2\big]}
$$

