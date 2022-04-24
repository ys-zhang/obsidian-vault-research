#statistics #classification #normal-distribution 


_LDA_ falls in the a class of discriminate function methods, i.e.
for each $k \in \{1, 2, \dots, K\}$ model a discriminate function $g_k(x)$ and the predicted $\hat G(x) = \arg\max_k g_k(x)$.


# Assumption

Assume the conditional expectation $E[x|G=k]$ is Gaussian that share same covariance matrix $\Sigma$. 

$$
  E[x|G=k] \sim \mathscr N(\mu_k, \Sigma) 
$$

under the assumption the parameter $\mu_k$ and $\Sigma$ can be easily estimated through sample mean and sample covariance.

$$
\ln \frac{\Pr(G=i|X=x)}{\Pr(G=K|X=x)} = c + x^T\Sigma^{-1}(\mu_i-\mu_K)
$$
is learner in $x$.


# Difference with Logistic regression

The difference with Logistic regression is the method of estimation.

