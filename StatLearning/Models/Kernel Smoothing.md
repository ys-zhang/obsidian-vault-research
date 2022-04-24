#statistics #impolation #LOESS #LOWESS #scatterplot-smoothing

> These memory based methods require in principle _little or no training_; all the work gets done at evaluation time.

> Kernel methods achieve flexibility by fitting simple models in a region local to the target point $x_0$. 


# Idea

The idea is to estimate the conditional expectation
$$
f(x_0) = E[Y|X=x_0]
$$
one non-parametric estimator is 
$$
\hat f(x_0) = \text{Avg}(y_i | x_i\in N(x_0))
$$
the problem is how to define the neighbourhood $N(x_0)$.


# Local Weighted Average

1. k-nearest
2. Nadaraya–Watson Kernel regression, $K_\lambda(x_1, x_2) = D(\frac{\|x_1-x_2\|}{\lambda})$.
$$
D(t) = \begin{cases}
  \frac{3}{4}(1-t^2) & |t|\le1  \\
  0  & |t|>1
\end{cases}
$$
  estimated value:
$$
\hat f(x) = \sum_{i=1}^n y_i \frac{K_\lambda(x_i, x)}{\sum_{j=1}^n K_\lambda(x_j, x)}
$$


![[Pasted image 20220421130447.png|Kernel Smoothers]]

>[!WARNING]
> Bias on the boundary due to asymmetry of the kernel on the boundary.
> This bias can be present in the interior of the domain as well, if the $X$ values are not equally spaced


# Local Linear Regression

$$
\text{Loss}(x_0) = \min_{\alpha(x_0),\;\beta(x_0)} \sum_i K_\lambda(x_0,x_i) \big[y_i-\alpha(x_0) - x^T\beta(x_0)\big]^2
$$

> Points far away from sample centroid has more influence on the linear model, which cancel out the error from imbalanced $X$ distribution 

Local linear regression automatically modifies the kernel to _correct the bias exactly to first order_, a phenomenon dubbed as _automatic kernel carpentry_.


# Local Likelihood

$$
\ell(x_0, \theta(x_0)) = \sum_i K_\lambda(x_0,x_i)\ell(y_i,x_i, \theta(x_0))
$$


# Kernel Density Classification

see [[Naive Bayes Classifiers]] and [[Kernel Density Estimators]].


# Kernel Regression

Give a kernel $K$ define the RBF basis

$$
f_k(x) = K_{\lambda_k}\big(\xi_k,x\big)
$$
where
- $\xi_k$ is the _location_ or _prototype_ parameter
- $\lambda_k$ is the _scale_ parameter

when 
- $K$ is the Gaussian Kernel it result in the **RBF network**.

**Renormalized radial basis function**:

$$
h_k(x) = \frac{D(\|x-\xi_k\|/\lambda)}{\sum_{i=1}^m D(\|x-\xi_i\|/\lambda)}
$$

The normalization avoid holes in the design space, _Nadaraya–Watson kernel_ regression estimator is an example where





