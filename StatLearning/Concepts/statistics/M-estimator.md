#statistics 

Analogy to MLE we minimize a general **loss function**

$$
\text{Loss}(\theta) = \sum_i \rho(x_i - \theta) 
$$
where 
- $x_i$ are **samples**
- $\theta$ is the **parameter to estimate**

The **M-estimator** is defined as 

$$
\hat \theta_M = \arg\min_\theta \text{Loss}(\theta)
$$

> The name of M-estimator suggests it is _maximum-likelihood-type_ estimators.

>[!EXAMPLE] Huber (Robust) Estimator
>
> $$ \rho(x) =  \begin{cases} \frac{1}{2} x^2   & |x| \le k \\ k|x| - \frac{1}{2}k^2 & |x| \ge k \end{cases} $$


Just like MLE, using [[The Delta Method]] it can prove the **M-estimator** has **asymptotic distribution**:

$$
\sqrt{n}(\hat\theta_M - \theta_0) \overset{d} \longrightarrow
\mathscr N(0, \frac{E_{\theta_0}\rho'(X-\theta_0)^2}{[E_{\theta_0}\rho''(X-\theta_0)]^2})
$$



