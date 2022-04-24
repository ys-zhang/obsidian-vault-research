#statistics #hypothesis-test 

The Model:

$$
y_{ij} = \mu_i + \varepsilon_{ij}
$$


Error distribution:
$$
\varepsilon_{ij} \sim N(\mu_i, \sigma_i^2)
$$

>[!NOTE] R
>
> see `t.test()` function in R


# Equal Variance $\sigma_1^2=\sigma_2^2$
Estimation of $\sigma^2$:

$$
\hat \sigma^2 = S^2_p = \frac{(n_1-1)S_1^2 + (n_2-1)S_2^2}{n_1+n_2 -2}
$$

Hypothesis:

$$
\begin{align}
H_0: \mu_1 = \mu_2 \\
H_1: \mu_1 \ne \mu_2
\end{align}
$$
Under $H_0$
$$
t_0 = \frac{\bar y_1 - \bar y_2}{\sqrt{S_p^2(\frac{1}{n_1} + \frac{1}{n_2})}}
\sim t_{n_1+n_2-2}
$$
notice $\bar y_1 - \bar y_2 \sim N(0, \sigma^2(1/n_1 + 1/n_2))$, notice the choice of $n_1$ and $n_2$ can affect the efficiency of the test.


> [!WARNING]
> Check the assumption of normal error and equal variance



# Unequal Variance $\sigma_1^2 \ne \sigma_2^2$

$$
t_0 = \frac{\bar y_1 - \bar y_2}{\sqrt{\frac{S_1^2}{n_1} + \frac{S_2^2}{n_2}}}
$$

**approximately** following $t_v$
$$
 v = \frac{(\frac{S_1^2}{n_1} + \frac{S_2^2}{n_2})^2}{\frac{(S_1^2/n_1)^2}{n_1-1} + \frac{(S_2^2/n_2)^2}{n_2-1}}
$$


# Z-Test (known variance)

$$
Z_0 = \frac{\bar y_1 - \bar y_2}{\sqrt{\frac{\sigma_1^2}{n_1} + \frac{\sigma_2^2}{n_2}}}
\sim N(0, 1)
$$