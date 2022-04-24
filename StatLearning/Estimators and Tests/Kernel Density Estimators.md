#statistics #nonparametric-statistics #summarize

# Empirical Distribution Estimator

**Empirical CDF:**
$$
F_n(x) = \frac{1}{n} \sum_{i=1}^n \mathbb I(X_i\le x) 
$$

**Rosenblatt Estimator**:

$$
\begin{align}
p_n^R(x) &= \frac{F_n(x+h)-F_n(x-h)}{2h} \\
&= \frac{1}{2nh} \sum_{i=1}^n \mathbb I(x-h\le X_i \le x+h)
 \\
&= \frac{1}{nh}\sum_{i=1}^n K_R\big (\frac{X_i-x}{h}\big)
\end{align}
$$

Generally the **Parzen–Rosenblatt estimator** or **kernel density estimator** is
$$
\hat p_n(x) = \frac{1}{nh} \sum_{i=1}^n K\big (\frac{X_i-x}{h}\big)
$$

where 
- $K$ is a **kernel**
$$
\int K(s)\;ds = 1
$$
- $h$ is the **bandwidth**



