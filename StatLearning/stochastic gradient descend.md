Consider an objective function $f(\theta)$
$$
f: \mathbb R^n \to \mathbb R
$$
and a random function $B(\theta)$, maps $\theta$ to some random variable with domain on $\mathbb R^n$
$$
B: \mathbb R^n \to r.v. \text{ on } \mathbb R^n
$$
such that 
$$
  E\big[ B(\theta_0) \big] = \nabla_\theta f(\theta_0)
$$
Then we have the following the theorem:

$$
\boldsymbol \theta^{(t+1)} = \boldsymbol\theta^{(t)} + \rho_t G_t\boldsymbol b_t(\boldsymbol \theta^{(t)})
$$
converges to local optimum if 

- $G_t\in \mathbb R^{n\times n}$ is _positive definite_ and eigenvalues of  $G_t$ is **bounded** over $t \in \mathbb N_+$. 
- $\sum \rho_t = \infty$  and $\sum \rho_t^2 < \infty$ 


