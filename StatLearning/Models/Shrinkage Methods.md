Shrinkage methods includes
- [[#Ridge Regression]]
- [[#Lasso]]



# Ridge Regression

>[!CITE] IDEA
>
>When there are many correlated variables in a linear regression model, their coefficients can become poorly determined and exhibit high variance.
> 
> A wildly large positive coefficient on one variable can be cancelled by a similarly large negative coefficient on its correlated cousin. 
> 
> By **imposing a size constraint on the coefficients**,  this problem is alleviated.

$$
\hat \beta_{\text{ridge}} = (X^TX + \lambda I)^{-1}X^Ty 
$$
> [!DANGER] Scaling
> 
> The ridge solutions are **not equivariant under scaling** of the inputs, and so one normally standardizes the inputs before solving the above equation.

let $X=UDV^T$ be the SVD of $X$, then

$$
\begin{align}
\hat y = X\hat\beta &= UD(D^2+\lambda I)^{-1}DU^Ty \\
&= \sum_{j=1}^p u_j {d_j^2\over d_j^2 + \lambda}u_j^Ty
\end{align}
$$
> This means that a greater amount of shrinkage is applied to the coordinates of basis vectors with smaller $d_j^2$.

>[!WARNING] no intercept penalty
>
>the intercept $\beta_0$ has been left out of the penalty term. Penalization of the intercept would make the procedure depend on the origin chosen for $y$;


## Equivalent representation
- equivalent to 
$$
\min_\beta\; \|y - X\beta\|^2 + \lambda \|\beta\|^2 
$$
- equivalent to a **normal prior** on $\beta \sim N(0, {\sigma^2\over\lambda})$
- equivalent to 
$$
\begin{align}
\min_\beta \; & (\beta-\hat\beta)^TX^TX(\beta-\hat \beta) \\
s.t. \; & \|\beta\| \le d
\end{align}
$$
Notice $(\hat \beta_{\text{ridge}}-\hat\beta)^TX^TX(\hat \beta_{\text{ridge}}-\hat \beta) = \|\hat y_{\text{ridge}} - \hat y\|^2$.
![[Pasted image 20220330114056.png]]

Let $\hat y_i$ be the LSE prediction of $y_i$ then $y_i - \hat y_i$ orthogonal to the column space of $X$. 
$$
  \|y - X\beta\|^2 = \|y - \hat y\|^2 + \| \hat y - X\beta \|^2 + 2(y-\hat y)^T\hat y
$$
thus to minimize the penalized $L_2$ error equivalent to 
$$
\min_\beta \;\; (\beta - \hat\beta)^TX^TX(\beta-\hat\beta) + \lambda\|\beta\|^2
$$
where $\hat\beta$ is MSE estimator.
[analysis - Why are additional constraint and penalty term equivalent in ridge regression? - Mathematics Stack Exchange](https://math.stackexchange.com/questions/335306/why-are-additional-constraint-and-penalty-term-equivalent-in-ridge-regression/336618#336618)


# Lasso


