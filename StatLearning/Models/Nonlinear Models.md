Differential equations that form the heart of the theory describing physical behaviour often lead to nonlinear models.
Nonlinear regression models are almost always deeply rooted in the appropriate science

$$
y = f(x;\theta) + \varepsilon
$$
- $\theta \in \mathbb R^{p+1}$ 
- $\varepsilon_i$ independent normal, $E[\varepsilon] = 0$, $var[\varepsilon] = \sigma^2$.
- $f$ is the **expectation function** and nonlinear in $\theta$, i.e., $\frac{\partial f}{\partial \theta}(\theta;x) = g(\theta;x)$ 


# Least Square Estimate

$$
\begin{align}
RSS(\theta) &= \sum_i \|y_i - f(x_i;\theta)\|^2 \\
\partial_\theta RSS(\theta) &\propto \sum_i (y_i - f(x_i;\theta))\partial_\theta f(x_i;\theta) 
\end{align}
$$
 
```ad-note
title: MLE

If the error terms in the model are normally and independently distributed with constant variance, application of the method of maximum likelihood to the estimation problem will lead to least squares.
```

# Transformation to Linear Model

A nonlinear model that can be transformed to an equivalent linear form is said to be **intrinsically linear**. However, the issue often revolves around the **error structure**, namely, do the standard assumptions on the errors apply to the original nonlinear model or to the linearised one? This is sometimes not an easy question to answer.


```ad-warning
the linear least squares estimates of the parameters in _transformed linear models_ will **not in general be equivalent to** the _nonlinear parameter estimates_ in the original model
```

a way to transform the model to linear is to use Taylor expansion on $f$ in $\theta$.
The first order expansion:

$$
y = \beta Z + \varepsilon
$$
where 
- $z_i = \partial_{\theta_i} f(x;\theta_0)$
