# Basic Data Transformation

## Stabilize Variance

![[Pasted image 20220324225451.png]]

A common reason for the violation of the **Constant Covariance** assumption is for the response variable $y$ to follow a probability distribution in which the variance is functionally related to the mean.

If this problem is not eliminated, the **least-squares estimators** will still be **unbiased**, but they will _no longer have the minimum variance property_. 

```ad-warning

It is often necessary to convert the predicted values back to the original units. 
Unfortunately, applying the _inverse transformation directly_ to the predicted values gives an estimate of the _median of the distribution_ of the response instead of the mean.
```

## Linearize the Model

Nonlinearity may be detected via the [[Linear Model#Lack of Fit test|Lack of Fit Test]]. 

![[Pasted image 20220325134105.png]]


## The Box-Cox (Power) y-Transform

Idea:

$$
  y \to y^\lambda
$$
however, to handle the singleton point at $0$, the transformation become

$$
y(\lambda) = \begin{cases}
\begin{align}
&\frac{y^\lambda -1}{\lambda (GM(y_{sample}))^{\lambda -1} } \; & \lambda \ne 0 \\
\\
&GM(y_{sample})\ln y \; & \lambda = 0
\end{align}
\end{cases}
$$
where $GM$ is the geometric mean of the samples.


```ad-note
MLE of $\lambda$ minimizes the training error.

This value of $\lambda$ is usually determined by fitting a model to $y(\lambda)$ for various values of $\lambda$ , plotting the residual sum of squares $SS_{Res}(\lambda)$ versus $\lambda$ , and then reading the value of $\lambda$ that minimizes $SS_{Res}(\lambda)$ from the graph. Usually 10–20 values of $\lambda$ are sufficient for estimation of the optimum value. 

```

## x-Transforms

Assumption:
- Normal and Independent responses
- Constant variance

given a parameterized transforms
$$
x \to f(x|a)
$$

to find the best $a$, using Taylor expansion

$$
\begin{align}
E(y) &= \beta_0 + \beta_1 f(x|a_1) \\
&= \beta_0 + \beta_1(f(x|a_0) + (a_1-a_0)\partial_af(x|a_0))
\end{align}
$$

just need to add a new regressor $\partial_af(x|a_0)$


