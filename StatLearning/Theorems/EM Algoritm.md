###### Problem
1. _Hidden Variable_

    sometime it's hard to model $p(x|\theta)$, however much easier to model $p(x|z)$ and $p(z|\theta)$.
2. _Missing Value_ or _Incomplete Data_

    for instance $x=(x_1, x_2)$ and $x_2$ is missing then the _incomplete data likelihood_ will be 
    $$
    L(\theta|x_1) = \int L(\theta|x_1, x_2)\;dx_2
  $$
    which is difficult to compute numerically.


##### Idea

Let 
- complete data likelihood: $L(\theta|x, z) = f(x, z|\theta)$
- incomplete data likelihood: $L(\theta|x) = f(x|\theta) = \int f(x, z|\theta)dz$
- $k(z|\theta, x) = \frac{f(x, z|\theta)}{f(x|\theta)}$

$$
\ell(\theta|x) = \ell(\theta|x, z) - \ln k(z|\theta, x)
$$
thus take expectation of $z$ suppose z under distribution given $x$ and $\theta'$ 

$$
\ell(\theta|x) = E[\ell(\theta|x, z)\; \big|\; \theta', x] - E_z[\ln k(z|\theta, x) \;\big |\; \theta', x]
$$
define the iteration

$$
\hat \theta_{n+1} = \arg \max_\theta E[\ell(\theta|x, z)\; | \; \hat\theta_n, x]
$$

EM-algorithm **convert** the problem of **computing incomplete data likelihood** ($\int f(x, z|\theta)dz$) to **iteration** based on the **complete data likelihood** $f(x, z|\theta)$ and conditional distribution of $z$ , i.e., $p(z|x, \theta)$


