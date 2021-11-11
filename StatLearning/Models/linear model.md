# Regression

input vector $x^T = (x_1, \dots, x_p)$ 
the linear regression model:

$$
  E[y|x] = f(x) = \beta_0 + x^T \beta_{1:p}
$$

while the [[RSS (residual sum of squares)]] is 

$$
	RSS = (Y - \hat{Y})^T(Y - \hat{Y}) = \sum_i (y_i - \beta_0 -\sum_j x_{ij}\beta_j)^2
$$

#### least of square
let $X \in \mathbb R_{N \times (p+1)}$ be the **design matrix**, *each row stands for a sample*.

the **least of square** is a method to estimate $\beta$ by minimize $RSS$.

$$
\hat \beta = (X^T X)^{-1}X^T Y
$$

and the prediction

$$
\hat Y = X\beta  = X(X^T X)^{-1}X^T Y
$$

The matrix $H = X(X^T X)^{−1}X^T$ appearing in equation is sometimes called the **hat matrix** because it puts the hat on $y$. The hat matrix $H$ computes the orthogonal projection, and hence it is also known as a **projection matrix**.


