 

A key _assumption_ in the GLM is that the _response variable distribution_ is a member of the [[Exponential Family]] of distributions.

suppose we have the follow nonlinear model:
$$
y = f(x^T\beta) + \varepsilon 
$$

$$
\begin{align}
\mu &= E[y] = f(x^T\beta) \\
g(\mu) &= f^{-1}(E[y]) = x^T\beta 
\end{align}
$$
the function $g = f^{-1}$ is named as **the link function** 


# Idea and Assumption

suppose $Y$ follows some [[Exponential Family]], i.e.,

$$
p(y|\theta, \sigma^2) = \exp\{ \frac{y^T\theta - A(\theta)}{\sigma^2} + B(y, \sigma^2) \}
$$
where
- $\theta$ is the **natural parameter**
- $\sigma^2$ is the **dispersion parameter**
- $B$ is a **normalisation constant**

Easy to see the distribution mean

$$
\mu = E[y] = \int p(y|\theta, \sigma^2) \; d \theta = f(\theta)
$$
Here $\mu = f(\theta) = A'(\theta)$ is uniquely determined by distribution of $Y$ 

